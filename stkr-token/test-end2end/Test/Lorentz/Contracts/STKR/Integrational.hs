module Test.Lorentz.Contracts.STKR.Integrational
  ( spec_NetworkTest
  ) where

import Prelude

import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime)
import Test.Hspec (Spec, it, runIO, shouldBe)

import Tezos.Address (Address, unsafeParseAddress)
import qualified Tezos.Core as Tz
import Tezos.Crypto (PublicKey, SecretKey, detSecretKey, hashKey, toPublic)
import TzTest (runTzTest)
import qualified TzTest as Tz

import Lorentz.Contracts.Client as Client
import qualified Lorentz.Contracts.Multisig as Msig
import qualified Lorentz.Contracts.Multisig.Client as Msig
import qualified Lorentz.Contracts.STKR as STKR
import qualified Lorentz.Contracts.STKR.Client as STKR


data TestOptions = TestOptions
  { faucet :: Address
  , msigAlias :: Text
  , tokenAlias :: Text
  , tzTestEnv :: Tz.Env
  }

newKeypair :: ByteString -> (SecretKey, PublicKey)
newKeypair bs = let sk = detSecretKey bs in (sk, toPublic sk)

spec_NetworkTest :: Spec
spec_NetworkTest = do
  timestamp <- runIO $ show <$> getCurrentTime
  let msigAlias = "msig-test" <> timestamp
  let tokenAlias = "alias-test" <> timestamp
  let faucet = unsafeParseAddress "tz1SPTYLWxfkee4CUxWfY5vuZCQvCVvj5RtS"
  tzTestEnv <- runIO $ Tz.readEnvFromFile "test-config.yaml"
  networkTestSpec $ TestOptions{..}

networkTestSpec :: TestOptions -> Spec
networkTestSpec TestOptions{..} = do
  let tzTest test = runTzTest test tzTestEnv
  let (sk1, pk1) = newKeypair "1"
  let (sk2, pk2) = newKeypair "2"
  let (sk3, pk3) = newKeypair "3"
  let (sk4, pk4) = newKeypair "4"
  let (sk5, pk5) = newKeypair "5"
  let (_, pk6) = newKeypair "6"
  let (_, pk7) = newKeypair "7"
  let teamKeys = Set.fromList $ hashKey <$> [pk1, pk2, pk3, pk4, pk5]
  let teamSks = [sk1, sk2, sk3, sk4, sk5]
  let newCouncilKeys = Set.fromList $ hashKey <$> [pk6, pk7]

  now <- runIO Tz.getCurrentTime
  let timeConfig = STKR.TestTC now 600

  let deployOpts =
        DeployOptions
          { originator = faucet
          , councilPks = []
          , ..
          }
  it "passes happy case for newCouncil" . tzTest $ do
    Client.DeployResult{..} <- deploy deployOpts
    let tokenParam = STKR.NewCouncil newCouncilKeys
    let currentNonce = 1
    chainId <- Tz.getMainChainId
    let order = Msig.mkCallOrderUnsafe tokenAddr tokenParam
    let toSign = Msig.ValueToSign
          { vtsChainId = chainId
          , vtsNonce = currentNonce
          , vtsOrder = order
          }
    let correctlySigned = Client.multisignValue teamSks toSign

    let msigParameter =
          Msig.Parameter
            { order = order
            , nonce = currentNonce
            , signatures = correctlySigned
            }
    Msig.call $
      Msig.CallOptions
        { caller = faucet
        , contract = msigAddr
        , parameter = msigParameter
        }
    STKR.Storage{..} <- STKR.getStorage tokenAddr
    lift $ councilKeys `shouldBe` newCouncilKeys
