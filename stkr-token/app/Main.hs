module Main
  ( main
  ) where

import Prelude

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml
import Fmt (pretty, (+|), (|+))
import qualified Lorentz as L
import qualified Options.Applicative as Opt
import Tezos.Crypto (hashKey, parsePublicKey)
import Util.IO (readFileUtf8, writeFileUtf8)

import TzTest (TzTest)
import qualified TzTest as Tz

import qualified Lorentz.Contracts.Client as Client
import Lorentz.Contracts.STKR (TimeConfig, stkrContract)
import Lorentz.Contracts.Multisig (multisigContract)
import qualified Lorentz.Contracts.STKR.Client as STKR


import Parser
  (CliCommand(..), DeployOptions(..), LocalCommand(..), RemoteAction(..), RemoteCommand(..),
  TzEnvConfig(..), cmdParser)

main :: IO ()
main = do
  (cmd, tc) <- Opt.execParser cmdParser
  case cmd of
    Local localCmd -> localCmdRunner tc localCmd
    Remote RemoteAction{..} -> do
      env <- case tzEnvConfig of
        YamlFile path -> Yaml.decodeFileThrow @IO @Tz.Env path
        CliArgs tzEnv -> pure tzEnv
      Tz.runTzTest (remoteCmdRunner tc remoteCmd) env

localCmdRunner :: TimeConfig -> LocalCommand -> IO ()
localCmdRunner tc = \case
    PrintMultisig out -> maybe putStrLn writeFileUtf8 out $ L.printLorentzContract False multisigContract
    PrintStkr out -> maybe putStrLn writeFileUtf8 out $ L.printLorentzContract False (stkrContract tc)

remoteCmdRunner :: TimeConfig -> RemoteCommand -> TzTest ()
remoteCmdRunner timeConfig = \case
  Deploy DeployOptions{..} -> do
    let readSkFromFile filename = readFileUtf8 filename >>= either (fail . pretty) pure . parsePublicKey . T.strip
    teamPks <- if null teamPksFiles
                then mapM (\i -> Tz.generateKey $ msigAlias <> "_key_" <> show (i :: Int)) [1..3]
                else liftIO $ traverse readSkFromFile teamPksFiles
    addrs <- Client.deploy $
      Client.DeployOptions
        { councilPks = []
        , teamKeys = Set.fromList $ hashKey <$> teamPks
        , ..
        }
    putStrLn @Text $ "Deploy result: " +| addrs |+ ""

  PrintStorage addr ->
    STKR.getStorage addr >>= liftIO . T.putStrLn . pretty
