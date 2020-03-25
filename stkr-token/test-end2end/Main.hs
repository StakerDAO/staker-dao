{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}

module Main
  ( main
  ) where

import Data.Aeson (FromJSON)
import Data.Time.Clock (getCurrentTime)
import qualified Data.Yaml as Yaml
import Lens.Micro (ix)
import System.Environment (getArgs)
import Test.Hspec.Core.Runner

import CryptoInterop (PublicKey, parseSecretKey)
import Tezos.Address (Address)
import TzTest (TzTest, runTzTest)
import qualified TzTest as Tz

import Test.Lorentz.Contracts.STKR.Integrational

data AccountData = AccountData
  { hash :: Address
  , publicKey :: PublicKey
  , secretKey :: Text
  }
  deriving stock Generic
  deriving anyclass FromJSON

loadTestAccout :: FilePath -> IO AccountData
loadTestAccout name = Yaml.decodeFileThrow @IO @AccountData name

importTestAccount :: Text -> FilePath -> TzTest Address
importTestAccount name path = do
  AccountData{..} <- liftIO $ loadTestAccout path
  sk <- either (\_ -> fail "Fail to parse secret key of imported account") pure (parseSecretKey secretKey)
  Tz.importSecretKey name sk

main :: IO ()
main = do
  timestamp <- show <$> getCurrentTime
  args <- getArgs
  tzTestEnv <- Tz.mkEnv $ args ^. ix 0
  faucet <- runTzTest (importTestAccount "faucet" $ args ^. ix 1) tzTestEnv
  let stageDuration = maybe (error "Fail to parse stage duration") id (readMaybe @Natural (args ^. ix 2))
  let spec = networkTestSpec $ TestOptions
        { msigAlias = "msig-test" <> timestamp
        , stkrAlias = "alias-test" <> timestamp
        , ..
        }
  runSpec spec defaultConfig >>= evaluateSummary
