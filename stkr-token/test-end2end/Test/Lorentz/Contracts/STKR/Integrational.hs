{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}

module Test.Lorentz.Contracts.STKR.Integrational
  ( spec_NetworkTest
  ) where

import Prelude

import Data.Aeson (FromJSON)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import qualified Data.Yaml as Yaml
import Test.Hspec (Spec, it, runIO, shouldBe)
import Control.Concurrent (threadDelay)
import Text.Hex (decodeHex)
import Michelson.Text (mkMTextUnsafe)
import Lorentz (lPackValue)

import Tezos.Address (Address)
import qualified Tezos.Core as Tz
import Tezos.Crypto (PublicKey, SecretKey, detSecretKey, hashKey, parseSecretKey, toPublic, blake2b)
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
  sk <- either (\_ -> fail "aaaa") pure (parseSecretKey secretKey)
  Tz.importSecretKey name sk

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
stageDuration = 150

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
  let timeConfig = STKR.TestTC start stageDuration
  let waitForStage (i :: Int) = do
        now <- Tz.timestampToSeconds <$> Tz.getCurrentTime
        let diff = Tz.timestampToSeconds start + i*stageDuration - now
        when (diff > 0) $ do
          putTextLn $ "Waiting for " <> show diff <> " seconds"
          threadDelay (diff * 1000000)

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
    lift $ waitForStage 0
    callViaMultisig (STKR.NewProposal newProposal) vmo
    STKR.Storage{..} <- STKR.getStorage stkrAddr
    let proposalAndHash =
          ( #proposal newProposal, #proposalHash $
            STKR.Blake2BHash $ blake2b $ lPackValue newProposal )
    lift $ proposals `shouldBe` [proposalAndHash]

  it "passes happy case for newCouncil" . tzTest $ do
    callViaMultisig (STKR.NewCouncil newCouncilKeys) vmo
    STKR.Storage{..} <- STKR.getStorage stkrAddr
    lift $ councilKeys `shouldBe` newCouncilKeys

  it "passes happy case for vote" . tzTest $ do
    lift $ waitForStage 2
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
    STKR.Storage{..} <- STKR.getStorage stkrAddr
    lift $ votes `shouldBe` Map.fromSet (const $ #proposalId 1) newCouncilKeys

