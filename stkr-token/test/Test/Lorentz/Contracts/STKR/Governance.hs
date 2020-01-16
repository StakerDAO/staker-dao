module Test.Lorentz.Contracts.STKR.Governance where
  -- ( spec_Governance
  -- ) where

import Prelude

import Control.Monad (replicateM)
import qualified Data.Map as Map
import Data.Time.Calendar (Day, addDays, addGregorianMonthsClip)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Lens.Micro (ix)
import qualified Lorentz as L
import Lorentz.Test
import Michelson.Text (mt)
import Named (arg)
import System.Random (Random)
import Test.Hspec (Expectation, Spec, describe, it, parallel, runIO)
import qualified Test.Hspec.QuickCheck as HQ
import Test.QuickCheck (Arbitrary(..), Property)
import Test.QuickCheck
  (Arbitrary(..), Gen, Testable(..), arbitrarySizedNatural, choose, conjoin,
  forAll, generate, once, sublistOf, vectorOf, withMaxSuccess)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary, genericArbitrary)
import Tezos.Core
  (Timestamp, parseTimestamp, timestampFromUTCTime, timestampPlusSeconds)
import Tezos.Crypto (SecretKey, blake2b, hashKey, sign, toPublic)
import Universum.Unsafe ((!!))
import Util.Named ((.!))

import qualified Lorentz.Contracts.STKR as STKR

import Test.Lorentz.Contracts.STKR.Common
  (OriginateParams(..), callSetSuccessor, callWithMultisig, expectSuccess,
  failWhenNot, newKeypair, originate, originateWithEmptyLedger,
  originateWithTC, wallet1)

data GovEpochDesc = GovEpochDesc
  { geNum :: Natural
  , geStages :: [Timestamp]
  , geProposalsWithVotes :: NonEmpty (STKR.Proposal, [SecretKey])
  , geWinner :: STKR.Proposal
  }
  deriving Show

genProposals :: (Int, Int) -> Gen [STKR.Proposal]
genProposals len = choose len >>= flip vectorOf (arbitrary @STKR.Proposal)

govEpochs :: Day -> [(Natural, [Timestamp])]
govEpochs firstEpochStart = startDayToEpoch <$> zip [0..] epochStartDays
  where
    epochStartDays = iterate (addGregorianMonthsClip 1) firstEpochStart
    dayToTimestampAtMidnight day =
      timestampFromUTCTime $ UTCTime day (secondsToDiffTime 0)
    startDayToEpoch (epochNum, startDay) =
      (epochNum,) $
        dayToTimestampAtMidnight <$>
          (flip addDays) startDay <$>
          [0, 7, 14, 21]


makeTestInput :: [SecretKey] -> IO [GovEpochDesc]
makeTestInput sks = do
  generated <- generate $ replicateM 100 $ generateTestInput sks
  let (nums, stages) = unzip testGovEpochs
  let (proposalsWithVotes, winner) = unzip generated
  pure . getZipList $
    GovEpochDesc
    <$> ZipList nums
    <*> ZipList stages
    <*> ZipList proposalsWithVotes
    <*> ZipList winner
  where
    firstEpochStartSample =
      fromMaybe (error $ "failed to parse firstEpochStart") $
        readMaybe @Day "2020-01-01"

    testGovEpochs = govEpochs firstEpochStartSample


splitNum :: (Random a, Num a) => a -> Int -> Gen [a]
splitNum n 0 = pure [n]
splitNum n size = do
  chosen <- choose (0, n)
  rest <- splitNum (n - chosen) (size - 1)
  pure $ chosen : rest

generateTestInput :: [SecretKey] -> Gen (NonEmpty (STKR.Proposal, [SecretKey]), STKR.Proposal)
generateTestInput sks = do
  let councilSize = length sks
  proposals <- (:|) <$> arbitrary <*> (sublistOf =<< vectorOf 2 arbitrary)
  (votes, winner) <- do
    let propNum = length proposals
    winner <- choose (0, propNum - 1)
    winnerGap <- choose (1, councilSize - councilSize `div` 2)
    let winnerVotes = councilSize `div` 2 + winnerGap
    let votesToDistribute = councilSize - winnerVotes
    votesWithoutWinner <- splitNum votesToDistribute (propNum - 1)
    let winnerProp = fromMaybe undefined $
            toList proposals ^? ix winner
    pure (insertAt winner winnerVotes votesWithoutWinner, winnerProp)
  when (length proposals /= length votes) $  error "props and votes mismatch"
  let keka = fst $ foldr (\votes (res, avSks) -> let (propSks, rest) = splitAt votes avSks in (propSks : res, rest)) ([], sks) votes
  let votesNe = fromMaybe (error "generateTestInput: empty") $ nonEmpty keka
  pure (zipNe proposals votesNe, winner)
  where
    zipNe (x :| xs) (y :| ys) = (x, y) :| zip xs ys
    insertAt i x xs =
      let (ls, rs) = splitAt i xs
      in ls ++ [x] ++ rs

--     insertAt i x xs = xs
--       -- let (xs, ys) = splitAt i xs
--       -- in xs ++ [x] ++ ys


integrationalForEpochs :: [SecretKey] -> (GovEpochDesc -> IntegrationalScenario) -> Spec
integrationalForEpochs sks expectation = do
  testInput <- runIO $ makeTestInput sks
  parallel $
    forM_  (zip [1..100] testInput) $ \(i, input) ->
      (it ("epoch " <> show i) . integrationalTestExpectation . expectation $ input)

integrationalForEpochsSeq :: [SecretKey] -> (GovEpochDesc -> IntegrationalScenario) -> Expectation
integrationalForEpochsSeq sks expectation = do
  testInput <- makeTestInput sks
  forM_  testInput $ \input ->
      (integrationalTestExpectation . expectation $ input)

integrationalForEpochsWithNext :: [SecretKey] -> ((GovEpochDesc, GovEpochDesc) -> IntegrationalScenario) -> Expectation
integrationalForEpochsWithNext sks expectation = do
  testInput <- makeTestInput sks
  forM_  (zip testInput (drop 1 testInput)) $ \input ->
      (integrationalTestExpectation . expectation $ input)

-- propertyForEpochs :: (GovEpochDesc -> Property) -> Property
-- propertyForEpochs prop =  conjoin $
--   (take 1 $ govEpochs firstEpochStartSample) <&> withMaxSuccess 5 . prop

originateWithProdTC teamPks councilPks =
  originateWithTC STKR.ProdTC { _startYear = 2020 } $
    OriginateParams
      { opTeamKeys = teamPks
      , opCouncilKeys = councilPks
      , opInitailLedger = mempty
      }

setStage :: GovEpochDesc -> Natural -> IntegrationalScenarioM ()
setStage epochDesc stage = do
  -- Hack to evaluate scenario part with given `now`
  -- because actual interpretation happens on validation step.
  -- This validation ensures than scenario part is evaluated with
  -- correct `now`.
  -- TODO: investigate into https://issues.serokell.io/issue/TM-218
  -- and updgrade morley dependency version as this would likely fix this issue
  validate . Right $ expectAnySuccess
  setNow $ getStageTs epochDesc stage

getStageTs :: GovEpochDesc -> Natural -> Timestamp
getStageTs GovEpochDesc{..} i =
  fromMaybe (error $ "failed to set stage " <> show i) $
    geStages ^? ix (fromInteger $ toInteger i)

voteForProposal
  :: L.ContractRef STKR.Parameter
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
          , cdContractAddr = L.fromContractAddr stkr
          , cdStageCounter = stageCounter
          }
  lCall stkr
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
    integrationalForEpochsSeq teamSks $ \epoch@GovEpochDesc{..} -> do

      (msig, stkr) <- originateWithProdTC teamPks []

      setStage epoch 1
      callWithMultisig msig 1 teamSks stkr . STKR.NewProposal $ geWinner
      validate . Left $
        lExpectCustomError #wrongStage (#stageCounter .! (geNum * 4 + 1))

  it "proposal submitted in the middle of stages 1, 2, 3 is rejected" $
    integrationalForEpochsSeq teamSks $ \epoch@GovEpochDesc{..} -> do
      (msig, stkr) <- originateWithProdTC teamPks []
      branchout $ [1..3] <&> (\testingAgainstStage ->
        ("for stage " <> show testingAgainstStage) ?- do
          setNow $
            let stageStart = getStageTs epoch testingAgainstStage
                secondsInDay = 60 * 60 * 24
            in timestampPlusSeconds stageStart $ 3 * secondsInDay
          callWithMultisig msig 1 teamSks stkr . STKR.NewProposal $ geWinner
          validate . Left $
            lExpectCustomError #wrongStage $
              #stageCounter .! (geNum * 4 + testingAgainstStage))

  it "if none of proposals received majority the policy doesn't change" $ do
    let councilKeys = [sk1, sk2, sk3, sk4]
    integrationalForEpochsWithNext councilKeys $ \(curEpoch, nextEpoch) -> do
      let props = fst <$> toList (geProposalsWithVotes curEpoch)
      if length props < 2
      then validate . Right $ expectAnySuccess
      else do
        (msig, stkr) <- originateWithProdTC teamPks $ toPublic <$> councilKeys
        setStage curEpoch 0
        forM_ (zip [1..] props) $ \(i, prop) ->
          callWithMultisig msig i teamSks stkr $ STKR.NewProposal prop

        setStage curEpoch 2
        forM_ [sk1, sk2] $ \sk ->
          voteForProposal stkr sk 0 (reverse props !! 0) (geNum curEpoch * 4 + 2)
        forM_ [sk3, sk4] $ \sk ->
          voteForProposal stkr sk 1 (reverse props !! 1) (geNum curEpoch * 4 + 2)

        setStage nextEpoch 0
        callWithMultisig msig (toEnum $ length props + 1) teamSks stkr $ STKR.NewProposal (geWinner nextEpoch)

        validate . Right . lExpectStorageUpdate stkr $ \STKR.Storage{..} ->
          failWhenNot (#urls Map.empty == policy) "Not equal"


  it "poposal successfully submitted in epoch 0" $
    integrationalForEpochsWithNext teamSks $ \(curEpoch, nextEpoch) -> do
      let props = fst <$> toList (geProposalsWithVotes curEpoch)
      (msig, stkr) <- originateWithProdTC teamPks teamPks
      setStage curEpoch 0
      forM_ (zip [1..] props) $ \(i, prop) ->
        callWithMultisig msig i teamSks stkr $ STKR.NewProposal prop


      branchout $
        [ "proposals in storage are correct" ?- do
            validate . Right . lExpectStorageUpdate stkr $ \STKR.Storage{..} -> do
              failWhenNot (reverse props == (arg #proposal . fst <$> proposals)) $
                "Submitted props: " <> show props <> "props in storage: " <> show proposals
        , "policy changes after successful vote" ?- do
            setStage curEpoch 2
            let withPropId =
                  zip
                  (reverse ([0..(toEnum $ length props - 1)] :: [Natural]))
                  (toList $ geProposalsWithVotes curEpoch)
            forM_ withPropId $
              \(proposalId, (proposal, voteSks)) ->
                forM_ voteSks $ \voteSk -> do
                  voteForProposal stkr voteSk proposalId proposal ((geNum curEpoch) * 4 + 2)

            let sksToProposals :: Map L.KeyHash ("proposalId" L.:! Natural)
                sksToProposals =
                  withPropId
                  & fmap (second $ fmap (hashKey . toPublic) . snd)
                  & concatMap (\(id, votesKeyHashes) -> (,id) <$> votesKeyHashes)
                  & fmap (second $ #proposalId)
                  & Map.fromList

            validate . Right . lExpectStorageUpdate stkr $ \STKR.Storage{..} -> do
              failWhenNot (votes == sksToProposals) $ "Votes distributed incorrectly"

            setStage nextEpoch 0
            callWithMultisig msig (toEnum @Natural $ length props + 1) teamSks stkr . STKR.NewProposal $ (geWinner nextEpoch)

            validate . Right . lExpectStorageUpdate stkr $ \STKR.Storage{..} ->
              failWhenNot (policy == (arg #newPolicy . snd . geWinner $ curEpoch)) "Policy set incorrectly"

        ]
        ++ ([0, 1, 3] <&> (\stage -> ("vote call fails during stage " <> show stage) ?- do
            let stageFromStart = geNum curEpoch * 4 + stage
            setStage curEpoch stage
            voteForProposal stkr sk1 0 (geWinner curEpoch) stageFromStart
            validate . Left $
              lExpectCustomError #wrongStage (#stageCounter stageFromStart)
        ))
