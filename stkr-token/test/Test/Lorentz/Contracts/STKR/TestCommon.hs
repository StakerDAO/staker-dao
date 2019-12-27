{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Lorentz.Contracts.STKR.TestCommon
  ( spec_ensureOwner
  , spec_getCurrentStage
  , spec_checkPkCanVote
  , spec_splitCounter
  , spec_calcWinner
  ) where

import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Functor.Identity (Identity(..))
import Lens.Micro (ix)

import Test.Hspec (Spec, it)
import Test.HUnit ((@?=), (@?), assertFailure)
import qualified Test.Hspec.QuickCheck as HQ
import Test.QuickCheck (Arbitrary(..), Gen)

import Tezos.Crypto (hashKey)
import Tezos.Core (Timestamp (..), timestampToSeconds, parseTimestamp)

import qualified Data.Time.Calendar as C
import qualified Data.Time.Clock as C
import qualified Data.Time.Clock.POSIX as C

import Michelson.Interpret (ContractEnv(..))
import Michelson.Text (MText, mkMTextUnsafe)

import Lorentz (KeyHash, (:!), Rec (..))
import qualified Lorentz as L
import Lorentz.Test

import Lorentz.Contracts.STKR.Storage
import Lorentz.Contracts.STKR.Governance
import Lorentz.Contracts.STKR.Misc

import Test.Lorentz.Contracts.STKR.Common (newKeypair)

newUrls :: [Int] -> Map MText (ByteString, MText)
newUrls = M.fromList . Prelude.map mkUrl
  where
    mkUrl i = ( [L.mt|resource#{i}|]
              , ( "hash" <> show i
                , [L.mt|https://example.com/resource#{i}|]
                ) )

testCouncilKeys :: [KeyHash]
testCouncilKeys = Prelude.map
    (hashKey . snd . newKeypair . {- BS.singleton -- univ -} one)
    [49 .. 53] -- "1" .. "5"

testStorage :: Storage
testStorage = Storage
  { owner = genesisAddress
  , councilKeys = (Set.fromList testCouncilKeys)
  , policy = #urls $ newUrls [1,2]
  , proposals = M.empty
  , lastProposalId = 0
  , votes = M.empty
  , stageCounter = 0
  , totalSupply = 0
  , ledger = L.BigMap M.empty
  , frozen = False
  , successor = Nothing
  }

spec_ensureOwner :: Spec
spec_ensureOwner = do
    it "ensureOwner" $ runEnsureOwner @?= Right True
  where
    runEnsureOwner = do
      let initStack =
            Identity (EnsureOwner (10 :: Natural))
              :& Identity testStorage :& RNil
      resStack <- L.interpretLorentzInstr dummyContractEnv ensureOwner initStack
      case resStack of
        Identity (10 :: Natural) :& RNil -> return True
        _ -> return False

spec_getCurrentStage :: Spec
spec_getCurrentStage = do
  it "test config" $
    forM_ [1, 10 .. now_] $ \s ->
    forM_ [1, 10 .. s] $ \sdur ->
      runTc s sdur @?= Right (fromInteger $ (now_ - s) `div` sdur)
  it "prod config: manual" $ do
    isLeft (runProd 2019 (parseTs "2018-12-30T00:00:00Z")) @? "before start"
    runProd 2019 (parseTs "2019-01-01T00:00:00Z") @?= Right 0
    runProd 2019 (parseTs "2019-01-07T23:59:59Z") @?= Right 0
    runProd 2019 (parseTs "2019-01-08T00:00:00Z") @?= Right 1
    runProd 2019 (parseTs "2019-01-09T00:00:00Z") @?= Right 1
    runProd 2019 (parseTs "2019-01-14T23:59:59Z") @?= Right 1
    runProd 2019 (parseTs "2019-01-15T00:00:00Z") @?= Right 2
    runProd 2019 (parseTs "2019-01-20T09:19:01Z") @?= Right 2
    runProd 2019 (parseTs "2019-01-21T23:59:59Z") @?= Right 2
    runProd 2019 (parseTs "2019-01-22T00:00:00Z") @?= Right 3
    runProd 2019 (parseTs "2019-01-28T23:59:59Z") @?= Right 3
    runProd 2019 (parseTs "2020-03-01T00:00:00Z") @?= Right (14 * 4)
    runProd 2019 (parseTs "2020-02-29T00:00:00Z") @?= Right (14 * 4 - 1)
  it "prod config: 2020..2030" $
    forM_ [2020..2030] $ \y ->
    forM_ [1..12] $ \m -> do
      let base = ((y - 2020) * 12 + m - 1) * 4
          ym = show y <> "-" <> (if m < 10 then "0" else "") <> show m
      runProd 2020 (parseTs $ ym <> "-01T00:00:00Z") @?= Right (base + 0)
      runProd 2020 (parseTs $ ym <> "-07T23:59:59Z") @?= Right (base + 0)
      runProd 2020 (parseTs $ ym <> "-08T00:00:00Z") @?= Right (base + 1)
      runProd 2020 (parseTs $ ym <> "-09T00:00:00Z") @?= Right (base + 1)
      runProd 2020 (parseTs $ ym <> "-14T23:59:59Z") @?= Right (base + 1)
      runProd 2020 (parseTs $ ym <> "-15T00:00:00Z") @?= Right (base + 2)
      runProd 2020 (parseTs $ ym <> "-20T09:19:01Z") @?= Right (base + 2)
      runProd 2020 (parseTs $ ym <> "-21T23:59:59Z") @?= Right (base + 2)
      runProd 2020 (parseTs $ ym <> "-22T00:00:00Z") @?= Right (base + 3)
      runProd 2020 (parseTs $ ym <> "-28T23:59:59Z") @?= Right (base + 3)
  HQ.prop "prod config: monotony" $ \tss_ f -> do
    let tss = sort (f : tss_)
    let firstTs = minimum tss
    let (startY, _, _) =
          C.toGregorian . C.utctDay .
          C.posixSecondsToUTCTime . unTimestamp $ firstTs
    res <- forM tss $ \ts ->
      case runProd (fromIntegral startY) ts of
        Left _ -> assertFailure $
          "error launching on startYear=" <> show startY
            <> " ts=" <> show ts
        Right r -> pure r
    let res' =
          case res of
            [] -> error "spec_getCurrentStage: unexpected empty"
            (a : as) ->
              foldl' (\b el -> b >>= \b_ ->
                  if b_ <= el
                  then Just el
                  else Nothing) (Just a) as
    when (isNothing res') $
      assertFailure "non-monotonic"
  where
    parseTs s = fromMaybe (error $ "spec_getCurrentStage: unexpected "
                <> "fail to parse timestamp: " <> s) (parseTimestamp s)
    now_ = timestampToSeconds $ ceNow dummyContractEnv
    runTc s sdur = do
      let tc = TestTC
            { _start = L.timestampFromSeconds s
            , _stageDuration = fromInteger sdur }
      resStack <- L.interpretLorentzInstr dummyContractEnv
        (getCurrentStage tc) RNil
      let Identity res :& RNil = resStack
      return res
    runProd _startYear ts = do
      let conf = ProdTC {..}
      resStack <- L.interpretLorentzInstr
        dummyContractEnv { ceNow = ts }
        (getCurrentStage conf) RNil
      let Identity res :& RNil = resStack
      return res

spec_checkPkCanVote :: Spec
spec_checkPkCanVote = do
    mapM_ (\pk -> it "checkPkCanVote" $ runCheckPkCanVote pk @?= Right True) testCouncilKeys
  where
    runCheckPkCanVote pk = do
      let initStack = (Identity pk :& Identity testStorage :& RNil)
      resStack <- L.interpretLorentzInstr dummyContractEnv (checkPkCanVote @'[]) initStack
      case resStack of RNil -> return True

instance Arbitrary Natural where
  arbitrary =
    (\i -> fromInteger $ if i <= 0 then 1-i else i)
    <$> (arbitrary :: Gen Integer)

spec_splitCounter :: Spec
spec_splitCounter =
    HQ.prop "splitCounter" $ \ctr -> let (e, s) = ctr `quotRem` 4 in runSplitCounter ctr @?= Right (Identity (#epoch e) :& Identity (#stage s) :& RNil)
  where
    runSplitCounter ctr = L.interpretLorentzInstr dummyContractEnv (splitCounter @'[]) (Identity ctr :& RNil)

spec_calcWinner :: Spec
spec_calcWinner = do
    it "no majority" $ run [0..4] [1, 0, 0, 1] @?= Right Nothing
    it "3/5" $ run [0..5] [1, 0, 0, 1, 0] @?= Right (Just $ #urls urls1)
    it "3/5 (two abstained)" $ run [1..4] [1, 1, 1] @?= Right (Just $ #urls urls2)
    it "4/5 (last proposal)" $ run [0..5] [2, 2, 2, 1, 2] @?= Right (Just $ #urls urls3)
  where
    urls1 = newUrls [3, 4]
    urls2 = newUrls [1, 4, 5]
    urls3 = newUrls [3, 4, 5]
    run voters pids = do
      let storage = testStorage
            { proposals = M.fromList $
                  [ (1, mkPrNHash "gago" urls1 "gagohash")
                  , (2, mkPrNHash "swin" urls2 "swinhash")
                  , (3, mkPrNHash "boro" urls3 "borohash")
                  ]
            , votes = mkVotes $ zip voters pids
            }
      let initStack = (Identity storage :& RNil)
      resStack <- L.interpretLorentzInstr dummyContractEnv (calcWinner @'[]) initStack
      let Identity p :& RNil = resStack
      return p
    (!!) l i = fromMaybe (error "spec_calcWinner: unexpected") $ l ^? ix i
    mkVotes :: [(Int, Natural)] -> Map KeyHash ("proposalId" :! Natural)
    mkVotes =
      M.fromList .
      Prelude.map (\(kndx, pid) ->
        (testCouncilKeys !! kndx, #proposalId pid) )
    mkPrNHash
      :: Text
      -> Map MText (ByteString, MText)
      -> ByteString
      -> ProposalAndHash
    mkPrNHash descr urls hash =
      ( #proposal ( #description $ mkMTextUnsafe descr
                  , #newPolicy $ #urls urls)
      , #proposalHash $ Blake2BHash hash)
