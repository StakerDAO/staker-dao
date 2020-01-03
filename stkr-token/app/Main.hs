module Main
  ( main
  ) where

import Prelude

import qualified Data.Set as Set
import qualified Data.Text.IO as T
import Fmt (pretty, (+|), (|+))
import qualified Lorentz as L
import qualified Options.Applicative as Opt
import qualified Data.Yaml as Yaml
import Tezos.Core (unsafeMkMutez)
import Tezos.Crypto (hashKey, parseKeyHash, formatPublicKey, formatSignature)
import Util.Named ((.!))
import Util.IO (writeFileUtf8)

import TzTest (TzTest)
import qualified TzTest as Tz

import qualified Lorentz.Contracts.Client as Client
import qualified Lorentz.Contracts.Multisig as Msig
import qualified Lorentz.Contracts.STKR as STKR
import qualified Lorentz.Contracts.STKR.Client as STKR

import Parser
  (CliCommand(..), DeployOptions(..), LocalCommand(..), NewCouncilOptions(..),
  NewProposalOptions(..), RemoteAction(..), RemoteCommand(..), TzEnvConfig(..),
  ViaMultisigOptions(..), VoteForProposalOptions(..), GetBalanceOptions(..),
  GetTotalSupplyOptions (..), TransferOptions (..), SetSuccessorOptions (..),
  WithdrawOptions (..), cmdParser)

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

signViaMultisig
  :: STKR.OpsTeamEntrypointParam
  -> ViaMultisigOptions
  -> TzTest (Msig.Parameter, [(L.PublicKey, L.Signature)])
signViaMultisig stkrParam ViaMultisigOptions {..} = do
  msigAddr <- Tz.resolve' Tz.ContractAlias vmoMsig
  stkrAddr <- Tz.resolve' Tz.ContractAlias vmoStkr
  Client.signViaMultisig stkrParam $ Client.ViaMultisigOptions
    { vmoMsig = msigAddr
    , vmoStkr = stkrAddr
    , vmoSign =
        \bytes ->
        mapM (Tz.resolve' (Tz.PkSigAlias bytes)) vmoMsigSignatures
    , ..
    }

printPkSigs
  :: STKR.OpsTeamEntrypointParam
  -> ViaMultisigOptions
  -> TzTest ()
printPkSigs stkrParam vmo = do
  (_, pkSigs) <- signViaMultisig stkrParam vmo
  forM_ pkSigs $ \(pk, sig) ->
    putTextLn $ formatPublicKey pk <> ":" <> formatSignature sig

callViaMultisig' ::
  (L.Address -> Client.ViaMultisigOptions -> TzTest ())
   -> ViaMultisigOptions -> TzTest ()
callViaMultisig' f ViaMultisigOptions {..} = do
  fromAddr <- maybe (fail "From address not specified")
                    (Tz.resolve' Tz.AddressAlias) vmoFrom
  msigAddr <- Tz.resolve' Tz.ContractAlias vmoMsig
  stkrAddr <- Tz.resolve' Tz.ContractAlias vmoStkr
  f fromAddr $ Client.ViaMultisigOptions
    { vmoMsig = msigAddr
    , vmoStkr = stkrAddr
    , vmoSign =
        \bytes ->
        mapM (Tz.resolve' (Tz.PkSigAlias bytes)) vmoMsigSignatures
    , ..
    }

callViaMultisig
  :: STKR.OpsTeamEntrypointParam -> ViaMultisigOptions -> TzTest ()
callViaMultisig p = callViaMultisig' (flip Client.callViaMultisig p)

remoteCmdRunner :: RemoteCommand -> TzTest ()
remoteCmdRunner = \case
  Deploy DeployOptions{..} -> do
    let parsePkHash =
          either (fail . pretty) pure . parseKeyHash
    let msigKeyName i = msigAlias <> "_key_" <> show (i :: Int)
    teamKeys <-
      fmap Set.fromList $
        if null teamPkHashes
        then mapM (fmap hashKey . Tz.generateKey . msigKeyName) [1..3]
        else traverse parsePkHash teamPkHashes
    originator' <- Tz.resolve' Tz.AddressAlias originator
    addrs <- Client.deploy $
      Client.DeployOptions
        { councilPks = []
        , originator = originator'
        , ..
        }
    putTextLn $ "Deploy result: " +| addrs |+ ""
  NewProposal NewProposalOptions {..} -> do
    prop <- liftIO $ STKR.fromJProposal <$> Yaml.decodeFileThrow npProposalFile
    if npPrintSigs
      then printPkSigs (STKR.NewProposal prop) npViaMultisig
      else callViaMultisig (STKR.NewProposal prop) npViaMultisig
  NewCouncil NewCouncilOptions {..} -> do
    let genCouncil (prefix, n) =
          mapM (\i -> fmap hashKey . Tz.generateKey $ prefix <> "_key_" <> show i) [1..n]
    council <- either (fmap Set.fromList . genCouncil) pure ncCouncil
    if ncPrintSigs
      then printPkSigs (STKR.NewCouncil council) ncViaMultisig
      else callViaMultisig (STKR.NewCouncil council) ncViaMultisig
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
  GetBalance GetBalanceOptions {..} -> do
    -- fromAddr <- Tz.resolve' Tz.AddressAlias gbFrom -- use getStorage API ATM
    stkrAddr <- Tz.resolve' Tz.ContractAlias gbStkr
    whoseAddr <- Tz.resolve' Tz.AddressAlias gbWhose
    Client.getBalance stkrAddr whoseAddr
  GetTotalSupply GetTotalSupplyOptions {..} -> do
    -- fromAddr <- Tz.resolve' Tz.AddressAlias gbFrom -- use getStorage API ATM
    stkrAddr <- Tz.resolve' Tz.ContractAlias gtsStkr
    Client.getTotalSupply stkrAddr
  Transfer TransferOptions {..} -> do
    tFromAddr <- Tz.resolve' Tz.AddressAlias tFrom
    tToAddr <- Tz.resolve' Tz.AddressAlias tTo
    callViaMultisig (STKR.Transfer (#from .! tFromAddr, #to .! tToAddr, #value .! tVal)) tViaMultisig
  Freeze msigOptions -> callViaMultisig (STKR.Freeze ()) msigOptions
  SetSuccessor SetSuccessorOptions {..} -> do
    ssSuccAddr <- Tz.resolve' Tz.ContractAlias ssSucc
    callViaMultisig' (flip Client.setSuccessor ssSuccAddr) ssViaMultisig
  Withdraw WithdrawOptions {..} -> do
    wFromAddr <- Tz.resolve' Tz.AddressAlias wFrom
    -- FIXME??? We use `unsafeMkMutez` which throws instead of manual handling of `Nothing`
    callViaMultisig' (\feePayer -> Client.withdraw feePayer wFromAddr $ unsafeMkMutez wAmount) wViaMultisig
