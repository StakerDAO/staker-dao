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
import Test.HUnit ((@?=))
import qualified Test.Hspec.QuickCheck as HQ
import Test.QuickCheck (Arbitrary(..), Gen)

import Tezos.Crypto (hashKey)
import Tezos.Core (timestampToSeconds)

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
  , proposals = []
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
      let initStack = (Identity (EnsureOwner (10 :: Natural)) :& Identity testStorage :& RNil)
      resStack <- L.interpretLorentzInstr dummyContractEnv (ensureOwner) initStack
      case resStack of (Identity (10 :: Natural) :& RNil) -> return True


spec_getCurrentStage :: Spec
spec_getCurrentStage =
  sequence_ [spec s sdur | s <- [1, 10 .. nows], sdur <- [1, 10 .. s]]
  where
    spec s sdur = it "getCurrentStage" $ runGetCurrentStage s sdur @?= Right (fromInteger $ (nows - s) `div` sdur)
    nows = timestampToSeconds $ ceNow dummyContractEnv
    runGetCurrentStage s sdur = do
      let tc = TestTC
            { _start = L.timestampFromSeconds s
            , _stageDuration = fromInteger sdur }
      resStack <- L.interpretLorentzInstr dummyContractEnv
        (getCurrentStage @'[] tc) RNil
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
            { proposals = [ mkPrNHash "gago" urls1 "gagohash"
                          , mkPrNHash "swin" urls2 "swinhash"
                          , mkPrNHash "boro" urls3 "borohash"
                          ]
            , votes = mkVotes $ zip voters pids
            }
      let initStack = (Identity storage :& RNil)
      resStack <- L.interpretLorentzInstr dummyContractEnv (calcWinner @'[]) initStack
      let Identity p :& RNil = resStack
      return p
    (!!) l i = fromMaybe (error "spec_calcWinner: unexpected") $ l ^? ix i
    mkVotes :: [(Int, Natural)] -> Map KeyHash ("proposalId" :! Natural)
    mkVotes = M.fromList . Prelude.map (\(kndx, pid) -> (testCouncilKeys !! kndx, #proposalId pid))
    mkPrNHash :: Text -> Map MText (ByteString, MText) -> ByteString -> ProposalAndHash
    mkPrNHash descr urls hash = (#proposal (#description $ mkMTextUnsafe descr, #newPolicy $ #urls urls), #proposalHash $ Blake2BHash hash)
