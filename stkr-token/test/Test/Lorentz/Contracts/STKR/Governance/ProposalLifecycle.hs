module Test.Lorentz.Contracts.STKR.Governance.ProposalLifecycle
  ( spec_ProposalLifecycle
  ) where

import Prelude

import qualified Data.Map as Map
import Lens.Micro (ix)
import qualified Lorentz as L
import Lorentz.CryptoInterop
  (KeyHash, PublicKey, SecretKey, blake2b, hashKey, sign, toPublic)
import Lorentz.Test
import Named (arg)
import Test.Hspec (Expectation, Spec, it)
import Test.QuickCheck (generate)
import Tezos.Core (Timestamp, timestampPlusSeconds)
import Universum.Unsafe ((!!))
import Util.Named ((:!), (.!))

import qualified Lorentz.Contracts.Multisig as Msig
import qualified Lorentz.Contracts.STKR as STKR

import Test.Lorentz.Contracts.STKR.Common
  (OriginateParams(..), callWithMultisig, failWhenNot, newKeypair,
  originateWithTC)
import Test.Lorentz.Contracts.STKR.Governance.Gen
  (EpochDesc, distribution, genEpochDesc, num, stages, votes, winner)

makeTestInput :: [SecretKey] -> IO [EpochDesc]
makeTestInput sks = generate $
  genEpochDesc sks 2020 1 100


integrationalForEpochsSeq :: [SecretKey] -> (EpochDesc -> IntegrationalScenario) -> Expectation
integrationalForEpochsSeq sks expectation = do
  testInput <- makeTestInput sks
  forM_  testInput $ \input ->
      (integrationalTestExpectation . expectation $ input)

integrationalForEpochsWithNext :: [SecretKey] -> ((EpochDesc, EpochDesc) -> IntegrationalScenario) -> Expectation
integrationalForEpochsWithNext sks expectation = do
  testInput <- makeTestInput sks
  forM_  (zip testInput (drop 1 testInput)) $ \input ->
      (integrationalTestExpectation . expectation $ input)


originateWithProdTC
  :: [PublicKey]
  -> [PublicKey]
  -> IntegrationalScenarioM
    ( L.TAddress Msig.Parameter
    , L.TAddress STKR.Parameter
    )
originateWithProdTC teamPks councilPks =
  originateWithTC STKR.ProdTC { _startYear = 2020 } $
    OriginateParams
      { opTeamKeys = teamPks
      , opCouncilKeys = councilPks
      , opInitailLedger = mempty
      }

-- Hack to evaluate scenario part with given `now`
-- because actual interpretation happens on validation step.
-- This validation ensures than scenario part is evaluated with
-- correct `now`.
-- TODO: investigate into https://issues.serokell.io/issue/TM-218
-- and updgrade morley dependency version as this would likely fix this issue
setStage :: EpochDesc -> Natural -> IntegrationalScenarioM ()
setStage epochDesc stage =
  validate (Right expectAnySuccess) >> setNow (getStageTs epochDesc stage)

getStageTs :: EpochDesc -> Natural -> Timestamp
getStageTs desc i =
  fromMaybe (error $ "failed to get stage " <> show i) $
    desc ^? stages . ix (fromInteger $ toInteger i)

voteForProposal
  :: L.TAddress STKR.Parameter
  -> SecretKey
  -> Natural
  -> STKR.Proposal
  -> Natural
  -> IntegrationalScenarioM ()
voteForProposal stkr voteSk proposalId proposal stageCounter = do
  let votePk = toPublic voteSk
  let bytesToSign = L.lPackValue $
        STKR.CouncilDataToSign
          { cdProposalHash = STKR.Blake2BHash . blake2b . L.lPackValue $ proposal
          , cdContractAddr = L.unTAddress stkr
          , cdStageCounter = stageCounter
          }
  lCallDef stkr
    . STKR.PublicEntrypoint
    . STKR.VoteForProposal $
    ( #proposalId .! proposalId
    , #votePk .! votePk
    , #voteSig .! sign voteSk bytesToSign
    )

spec_ProposalLifecycle :: Spec
spec_ProposalLifecycle = do
  let (sk1, _) = newKeypair "1"
  let (sk2, _) = newKeypair "2"
  let (sk3, _) = newKeypair "3"
  let (sk4, _) = newKeypair "4"
  let (sk5, _) = newKeypair "5"
  let teamSks = [sk1, sk2, sk3, sk4, sk5]
  let teamPks = toPublic <$> teamSks

  it "proposal submitted on boundary of stage 0 and 1 is rejected" $
    integrationalForEpochsSeq teamSks $ \epoch -> do

      (msig, stkr) <- originateWithProdTC teamPks []

      setStage epoch 1
      callWithMultisig msig 1 teamSks stkr . STKR.NewProposal $ epoch^.votes.winner._1
      validate . Left $
        lExpectCustomError #wrongStage (#stageCounter .! (epoch^.num * 4 + 1))

  it "proposal submitted in the middle of stages 1, 2, 3 is rejected" $
    integrationalForEpochsSeq teamSks $ \epoch -> do
      (msig, stkr) <- originateWithProdTC teamPks []
      branchout $ [1..3] <&> (\testingAgainstStage ->
        ("for stage " <> show testingAgainstStage) ?- do
          setNow $
            let stageStart = getStageTs epoch testingAgainstStage
                secondsInDay = 60 * 60 * 24
            in timestampPlusSeconds stageStart $ 3 * secondsInDay
          callWithMultisig msig 1 teamSks stkr . STKR.NewProposal $ epoch^.votes.winner._1
          validate . Left $
            lExpectCustomError #wrongStage $
              #stageCounter .! (epoch^.num * 4 + testingAgainstStage))

  it "if none of proposals received majority the policy doesn't change" $ do
    let councilKeys = [sk1, sk2, sk3, sk4]
    integrationalForEpochsWithNext councilKeys $ \(curEpoch, nextEpoch) -> do
      let props = fst <$> curEpoch^.votes.distribution
      if length props < 2
      then validate . Right $ expectAnySuccess
      else do
        (msig, stkr) <- originateWithProdTC teamPks $ toPublic <$> councilKeys
        setStage curEpoch 0
        forM_ (zip [1..] props) $ \(i, prop) ->
          callWithMultisig msig i teamSks stkr $ STKR.NewProposal prop

        setStage curEpoch 2
        forM_ [sk1, sk2] $ \sk ->
          voteForProposal stkr sk 0 (reverse props !! 0) (curEpoch^.num * 4 + 2)
        forM_ [sk3, sk4] $ \sk ->
          voteForProposal stkr sk 1 (reverse props !! 1) (curEpoch^.num * 4 + 2)

        setStage nextEpoch 0
        callWithMultisig msig (toEnum $ length props + 1) teamSks stkr $ STKR.NewProposal (nextEpoch^.votes.winner._1)

        validate . Right . lExpectStorageUpdate stkr $ \storage ->
          failWhenNot (#urls Map.empty == STKR.policy storage) "Not equal"


  it "poposal successfully submitted in epoch 0" $
    integrationalForEpochsWithNext teamSks $ \(curEpoch, nextEpoch) -> do
      let props = fst <$> curEpoch^.votes.distribution
      (msig, stkr) <- originateWithProdTC teamPks teamPks
      setStage curEpoch 0
      forM_ (zip [1..] props) $ \(i, prop) ->
        callWithMultisig msig i teamSks stkr $ STKR.NewProposal prop


      branchout $
        [ "proposals in storage are correct" ?- do
            validate . Right . lExpectStorageUpdate stkr $ \storage -> do
              failWhenNot (reverse props == (arg #proposal . fst <$> STKR.proposals storage)) $
                "Submitted props: " <> show props <> "props in storage: " <> show (STKR.proposals storage)
        , "policy changes after successful vote" ?- do
            setStage curEpoch 2
            let withPropId =
                  zip
                  (reverse ([0..(toEnum $ length props - 1)] :: [Natural]))
                  (curEpoch^.votes.distribution)
            forM_ withPropId $
              \(proposalId, (proposal, voteSks)) ->
                forM_ voteSks $ \voteSk -> do
                  voteForProposal stkr voteSk proposalId proposal (curEpoch^.num * 4 + 2)

            let sksToProposals :: Map KeyHash ("proposalId" :! Natural)
                sksToProposals =
                  withPropId
                  & fmap (second $ fmap (hashKey . toPublic) . snd)
                  & concatMap (\(propId, votesKeyHashes) -> (,propId) <$> votesKeyHashes)
                  & fmap (second $ #proposalId)
                  & Map.fromList

            validate . Right . lExpectStorageUpdate stkr $ \storage -> do
              failWhenNot (STKR.votes storage == sksToProposals) $ "Votes distributed incorrectly"

            setStage nextEpoch 0
            callWithMultisig msig (toEnum @Natural $ length props + 1) teamSks stkr . STKR.NewProposal $ nextEpoch^.votes.winner._1

            validate . Right . lExpectStorageUpdate stkr $ \storage ->
              failWhenNot (STKR.policy storage == (arg #newPolicy $ curEpoch^.votes.winner._1._2)) "Policy set incorrectly"

        ]
        ++ ([0, 1, 3] <&> (\stage -> ("vote call fails during stage " <> show stage) ?- do
            let stageFromStart = curEpoch^.num * 4 + stage
            setStage curEpoch stage
            voteForProposal stkr sk1 0 (curEpoch^.votes.winner._1) stageFromStart
            validate . Left $
              lExpectCustomError #wrongStage (#stageCounter stageFromStart)
        ))
