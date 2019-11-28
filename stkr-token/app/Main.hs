module Main
  ( main
  ) where

import Prelude

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml
import Fmt (pretty)
import qualified Lorentz as L
import qualified Options.Applicative as Opt
import Tezos.Address (formatAddress)
import Tezos.Crypto (parsePublicKey)
import Util.IO (readFileUtf8, writeFileUtf8)

import TzTest (TzTest)
import qualified TzTest as Tz

import Lorentz.Contracts.STKR (stkrContract)
import qualified Lorentz.Contracts.STKR.Client as Client

import Parser
  (CliCommand(..), DeployOptions(..), LocalCommand(..), RemoteAction(..), RemoteCommand(..),
  TzEnvConfig(..), cmdParser)

main :: IO ()
main = do
  cmd <- Opt.execParser cmdParser
  case cmd of
    Local localCmd -> localCmdRunner localCmd
    Remote RemoteAction{..} -> do
      env <- case tzEnvConfig of
        YamlFile path -> Yaml.decodeFileThrow @IO @Tz.Env path
        CliArgs tzEnv -> pure tzEnv
      Tz.runTzTest (remoteCmdRunner remoteCmd) env

localCmdRunner :: LocalCommand -> IO ()
localCmdRunner = \case
    PrintContract out -> maybe putStrLn writeFileUtf8 out $ L.printLorentzContract True stkrContract

remoteCmdRunner :: RemoteCommand -> TzTest ()
remoteCmdRunner = \case
  Deploy DeployOptions{..} -> do
    let readSkFromFile filename = readFileUtf8 filename >>= either (fail . pretty) pure . parsePublicKey . T.strip
    teamPks <- if null teamPksFiles
                then mapM (\i -> Tz.generateKey $ contractAlias <> "_key_" <> show (i :: Int)) [1..3]
                else traverse (liftIO . readSkFromFile) teamPksFiles
    addr <- Client.deploy $
      Client.DeployOptions
        { councilPks = []
        , ..
        }
    putStrLn $ "Contract addr: " <> formatAddress addr

  PrintStorage addr ->
    Client.getStorage addr >>= liftIO . T.putStrLn . pretty
