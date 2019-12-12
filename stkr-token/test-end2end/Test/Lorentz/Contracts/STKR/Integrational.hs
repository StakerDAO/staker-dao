{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}

module Test.Lorentz.Contracts.STKR.Integrational
  ( spec_NetworkTest
  ) where

import Prelude

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import qualified Data.Yaml as Yaml
import Fmt ((+|), (|+))
import Lorentz (lPackValue)
import Michelson.Text (mkMTextUnsafe)
import Test.Hspec (Expectation, Spec, it, runIO, shouldBe)
import Text.Hex (decodeHex)

import Tezos.Address (Address)
import qualified Tezos.Core as Tz
import Tezos.Crypto (PublicKey, SecretKey, blake2b, detSecretKey, hashKey, parseSecretKey, toPublic)
import TzTest (TzTest, runTzTest)
import qualified TzTest as Tz

import Lorentz.Contracts.Client as Client
import qualified Lorentz.Contracts.STKR as STKR
import qualified Lorentz.Contracts.STKR.Client as STKR


data TestOptions = TestOptions
  { faucetName :: Text
  , msigAlias :: Text
  , stkrAlias :: Text
  , tzTestEnv :: Tz.Env
  }

data AccountData = AccountData
  { hash :: Address
  , publicKey :: PublicKey
  , secretKey :: Text
  }
  deriving stock Generic
  deriving anyclass FromJSON

newKeypair :: ByteString -> (SecretKey, PublicKey)
newKeypair bs = let sk = detSecretKey bs in (sk, toPublic sk)

loadTestAccount :: Text -> IO AccountData
loadTestAccount name =
  Yaml.decodeFileThrow @IO @AccountData (T.unpack $ "test-accounts/" <> name <> ".yaml")

importTestAccount :: Text -> TzTest Address
importTestAccount name = do
  AccountData{..} <- liftIO $ loadTestAccount name
  sk <- either
    (\e -> fail $ "Error importing test account "+|name|+": "+|e|+"") pure $
    parseSecretKey secretKey
  Tz.importSecretKey name sk

expectStorage :: Address -> (STKR.AlmostStorage -> Expectation) -> TzTest ()
expectStorage addr check = STKR.getStorage addr >>= lift . check

spec_NetworkTest :: Spec
spec_NetworkTest = do
  timestamp <- runIO $ show <$> getCurrentTime
  tzTestEnv <- runIO $ Tz.readEnvFromFile "test-config.yaml"
  networkTestSpec $ TestOptions
    { faucetName = "faucet"
    , msigAlias = "msig-test" <> timestamp
    , stkrAlias = "stkr-test" <> timestamp
    , ..
    }

tezosWpUrlHash :: (STKR.Hash, STKR.URL)
tezosWpUrlHash = (hash_, url)
  where
    url = mkMTextUnsafe "https://tezos.com/static/white_paper-2dc8c02267a8fb86bd67a108199441bf.pdf"
    hash_ = fromMaybe (error "tezosWpUrlHash: unexpected") . decodeHex $
      "be7663e0ef87d51ab149a60dfad4df5940d30395ba287d9907f8d66ce5061d96"

stageDuration :: Num a => a
stageDuration = 500

networkTestSpec :: TestOptions -> Spec
networkTestSpec TestOptions{..} = do
  let tzTest :: TzTest a -> IO a
      tzTest test = runTzTest test tzTestEnv
  let (sk1, pk1) = newKeypair "1"
  let (sk2, pk2) = newKeypair "2"
  let (sk3, pk3) = newKeypair "3"
  let (sk4, pk4) = newKeypair "4"
  let (sk5, pk5) = newKeypair "5"
  let (sk6, pk6) = newKeypair "6"
  let (sk7, pk7) = newKeypair "7"
  let teamKeys = Set.fromList $ hashKey <$> [pk1, pk2, pk3, pk4, pk5]
  let teamSks = [sk1, sk2, sk3, sk4, sk5]
  let newCouncilKeys = Set.fromList $ hashKey <$> [pk6, pk7]

  start <- flip Tz.timestampPlusSeconds 100 <$> runIO Tz.getCurrentTime
  let startSeconds = Tz.timestampToSeconds start
  let timeConfig = STKR.TestTC start stageDuration

  let waitForStage (i :: Int) = putTextLn ("Waiting for stage "+|i|+"...") >> loop
        where
          waitTill = startSeconds + i*stageDuration
          pollCycleDuration = 2000000 -- 2 seconds
          loop = do
            headTime <- Tz.timestampToSeconds <$> Tz.getHeadTimestamp
            now <- lift $ Tz.timestampToSeconds <$> Tz.getCurrentTime
            let networkLag = now - headTime
            when (networkLag > stageDuration
                  && networkLag < stageDuration + pollCycleDuration) $ do
              putTextLn $ "Network lag is "+|networkLag|+
                " which is bigger than stage duration."
              putTextLn $ "This test is likely to fail :("
            if headTime < waitTill
            then lift (threadDelay pollCycleDuration) >> loop
            else do
              putTextLn $ "Stage "+|i|+" block reached chain "+|now - startSeconds|+
                " seconds after start."
              putTextLn $ "Testing stage "+|i|+"..."

  let newUrls = Map.singleton (mkMTextUnsafe "tezos-wp") tezosWpUrlHash
  let newProposal = ( #description (mkMTextUnsafe "First")
                    , #newPolicy (#urls newUrls))
  faucet <- runIO $ tzTest $ importTestAccount faucetName

  runIO $ putTextLn "Deploying contracts..."
  ContractAddresses{..} <-
    runIO $ tzTest $ deploy $
    DeployOptions
      { originator = faucet
      , councilPks = []
      , ..
      }

  let vmo =
        ViaMultisigOptions
          { vmoFrom = faucet
          , vmoMsig = msigAddr
          , vmoStkr = stkrAddr
          , vmoSign = pure . multisignBytes teamSks
          , vmoNonce = Nothing
          }

  it "passes happy case for newProposal" . tzTest $ do
    waitForStage 0
    callViaMultisig #cNewProposal newProposal vmo
    expectStorage stkrAddr $ \STKR.AlmostStorage{..} -> do
      let proposalAndHash =
            ( #proposal newProposal, #proposalHash $
              STKR.Blake2BHash $ blake2b $ lPackValue newProposal )
      proposals `shouldBe` [proposalAndHash]

    callViaMultisig #cNewCouncil newCouncilKeys vmo
    expectStorage stkrAddr $ \STKR.AlmostStorage{..} ->
      councilKeys `shouldBe` newCouncilKeys

    waitForStage 2
    let vpo =
          VoteForProposalOptions
            { vpEpoch = 0
            , vpProposalId = 1
            , vpFrom = faucet
            , vpStkr = stkrAddr
            , vpSign = pure . signBytes sk6
            }
    voteForProposal vpo
    voteForProposal vpo {vpSign = pure . signBytes sk7}
    expectStorage stkrAddr $ \STKR.AlmostStorage{..} ->
      votes `shouldBe` Map.fromSet (const $ #proposalId 1) newCouncilKeys
