module Main
  ( main
  ) where

import Prelude

import Named (arg)
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
  (CliCommand(..), DeployOptions(..), LocalCommand(..), RemoteAction(..), RemoteCommand(..),
  TzEnvConfig(..), NewCouncilOptions (..), NewProposalOptions (..), VoteForProposalOptions (..),
  ViaMultisigOptions (..), cmdParser)

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

callViaMultisig :: STKR.Parameter -> ViaMultisigOptions -> TzTest ()
callViaMultisig stkrParam ViaMultisigOptions {..} = do
  fromAddr <- Tz.resolve' Tz.AddressAlias vmoFrom
  msigAddr <- Tz.resolve' Tz.ContractAlias vmoMsig
  stkrAddr <- Tz.resolve' Tz.ContractAlias vmoStkr
  let order = Msig.mkCallOrderUnsafe stkrAddr stkrParam
  chainId <- Tz.getMainChainId
  let getNonce = (+1) . Msig.currentNonce <$> Tz.getStorage msigAddr
  nonce <- maybe getNonce pure vmoNonce
  let toSign = Msig.ValueToSign chainId nonce order
  let bytes = L.lPackValue toSign
  pkSigs <- mapM (Tz.resolve' (Tz.PkSigAlias bytes)) vmoMsigSignatures
  let param = Msig.Parameter order nonce pkSigs
  Tz.call fromAddr msigAddr param

remoteCmdRunner :: RemoteCommand -> TzTest ()
remoteCmdRunner = \case
  Deploy DeployOptions{..} -> do
    let readSkFromFile filename = readFileUtf8 filename >>= either (fail . pretty) pure . parsePublicKey . T.strip
    teamPks <- if null teamPksFiles
                then mapM (\i -> Tz.generateKey $ msigAlias <> "_key_" <> show (i :: Int)) [1..3]
                else liftIO $ traverse readSkFromFile teamPksFiles
    originator' <- Tz.resolve' Tz.AddressAlias originator
    addrs <- Client.deploy $
      Client.DeployOptions
        { councilPks = []
        , teamKeys = Set.fromList $ hashKey <$> teamPks
        , originator = originator'
        , ..
        }
    putTextLn $ "Deploy result: " +| addrs |+ ""
  NewProposal NewProposalOptions {..} ->
    callViaMultisig (STKR.NewProposal npProposal) npViaMultisig
  NewCouncil NewCouncilOptions {..} -> do
    let genCouncil (prefix, n) =
          mapM (\i -> fmap hashKey . Tz.generateKey $ prefix <> "_key_" <> show i) [1..n]
    council <- either (fmap Set.fromList . genCouncil) pure ncCouncil
    callViaMultisig (STKR.NewCouncil council) ncViaMultisig
  VoteForProposal VoteForProposalOptions {..} -> do
    fromAddr <- Tz.resolve' Tz.AddressAlias vpFrom
    stkrAddr <- Tz.resolve' Tz.ContractAlias vpStkr
    storage <- Tz.getStorage stkrAddr
    proposalHash <-
      maybe (fail $ "Proposal id not found " <> show vpProposalId) pure .
      fmap (arg #proposalHash . snd) . safeHead . snd . splitAt (fromIntegral vpProposalId - 1) $
      STKR.proposals storage
    let curStage = vpEpoch*4 + 2
    let toSignB = L.lPackValue $ STKR.CouncilDataToSign proposalHash stkrAddr curStage
    (pk, sig) <- Tz.resolve' (Tz.PkSigAlias toSignB) vpPkSig
    Tz.call fromAddr stkrAddr $ STKR.VoteForProposal
      (#proposalId vpProposalId, #votePk pk, #voteSig sig)
  PrintStorage addr_ ->
    Tz.resolve' Tz.ContractAlias addr_ >>=
    STKR.getStorage >>= liftIO . T.putStrLn . pretty
