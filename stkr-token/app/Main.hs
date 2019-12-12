module Main
  ( main
  ) where

import Prelude

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Fmt (pretty, (+|), (|+))
import qualified Lorentz as L
import qualified Options.Applicative as Opt
import Tezos.Crypto (hashKey, parsePublicKey)
import Util.IO (readFileUtf8, writeFileUtf8)

import TzTest (TzTest)
import qualified TzTest as Tz

import qualified Lorentz.Contracts.Client as Client
import qualified Lorentz.Contracts.Multisig as Msig
import qualified Lorentz.Contracts.STKR as STKR
import qualified Lorentz.Contracts.STKR.Client as STKR


import Parser
  (CliCommand(..), DeployOptions(..), LocalCommand(..), NewCouncilOptions(..),
  NewProposalOptions(..), RemoteAction(..), RemoteCommand(..), TzEnvConfig(..),
  ViaMultisigOptions(..), VoteForProposalOptions(..), cmdParser)

main :: IO ()
main = do
  cmd <- Opt.execParser cmdParser
  case cmd of
    Local localCmd -> localCmdRunner localCmd
    Remote RemoteAction{..} -> do
      env <- case tzEnvConfig of
        YamlFile path -> Tz.readEnvFromFile path
        CliArgs tzEnv -> pure tzEnv
      Tz.runTzTest (remoteCmdRunner remoteCmd) env

localCmdRunner :: LocalCommand -> IO ()
localCmdRunner = \case
    PrintMultisig out ->
      maybe putStrLn writeFileUtf8 out $
      L.printLorentzContract False Msig.multisigContract
    PrintStkr out tc ->
      maybe putStrLn writeFileUtf8 out $
      L.printLorentzContract False (STKR.stkrContract tc)

callViaMultisig
  :: Msig.TransferOrderWrapC STKR.Parameter cName it
  => Msig.Label cName -> it -> ViaMultisigOptions -> TzTest ()
callViaMultisig label stkrParam ViaMultisigOptions {..} = do
  fromAddr <- Tz.resolve' Tz.AddressAlias vmoFrom
  msigAddr <- Tz.resolve' Tz.ContractAlias vmoMsig
  stkrAddr <- Tz.resolve' Tz.ContractAlias vmoStkr
  Client.callViaMultisig label stkrParam $ Client.ViaMultisigOptions
    { vmoFrom = fromAddr
    , vmoMsig = msigAddr
    , vmoStkr = stkrAddr
    , vmoSign =
        \bytes ->
        mapM (Tz.resolve' (Tz.PkSigAlias bytes)) vmoMsigSignatures
    , ..
    }

remoteCmdRunner :: RemoteCommand -> TzTest ()
remoteCmdRunner = \case
  Deploy DeployOptions{..} -> do
    let readSkFromFile filename =
          readFileUtf8 filename >>=
          either (fail . pretty) pure . parsePublicKey . T.strip
    let msigKeyName i = msigAlias <> "_key_" <> show (i :: Int)
    teamKeys <-
      fmap (Set.fromList . fmap hashKey) $
        if null teamPksFiles
        then mapM (Tz.generateKey . msigKeyName) [1..3]
        else liftIO $ traverse readSkFromFile teamPksFiles
    originator' <- Tz.resolve' Tz.AddressAlias originator
    addrs <- Client.deploy $
      Client.DeployOptions
        { councilPks = []
        , originator = originator'
        , ..
        }
    putTextLn $ "Deploy result: " +| addrs |+ ""
  NewProposal NewProposalOptions {..} ->
    callViaMultisig #cNewProposal npProposal npViaMultisig
  NewCouncil NewCouncilOptions {..} -> do
    let genCouncil (prefix, n) =
          mapM (\i -> fmap hashKey . Tz.generateKey $ prefix <> "_key_" <> show i) [1..n]
    council <- either (fmap Set.fromList . genCouncil) pure ncCouncil
    callViaMultisig #cNewCouncil council ncViaMultisig
  VoteForProposal VoteForProposalOptions {..} -> do
    fromAddr <- Tz.resolve' Tz.AddressAlias vpFrom
    stkrAddr <- Tz.resolve' Tz.ContractAlias vpStkr
    Client.voteForProposal $ Client.VoteForProposalOptions
      { vpFrom = fromAddr
      , vpStkr = stkrAddr
      , vpSign = \toSignB -> Tz.resolve' (Tz.PkSigAlias toSignB) vpPkSig
      , ..
      }
  PrintStorage addr_ ->
    Tz.resolve' Tz.ContractAlias addr_ >>=
    STKR.getStorage >>= liftIO . T.putStrLn . pretty
