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
import Util.Named ((.!))
import Tezos.Crypto (hashKey, formatPublicKey, formatSignature)
import Util.IO (writeFileUtf8)
import Lorentz.Value (toContractRef, Address)

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
  WithdrawOptions (..), FreezeOptions (..),
  RotateMsigKeysOptions (..), cmdParser)

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
  :: Msig.Order
  -> ViaMultisigOptions
  -> TzTest (Address, (Msig.Parameter, [(L.PublicKey, L.Signature)]))
signViaMultisig order ViaMultisigOptions {..} = do
  msigAddr <- Tz.resolve' Tz.ContractAlias vmoMsig
  (msigAddr, ) <$> (Client.signViaMultisig order $ Client.ViaMultisigOptions
    { vmoMsig = msigAddr
    , vmoSign =
        \bytes ->
        mapM (Tz.resolve' (Tz.PkSigAlias bytes)) vmoMsigSignatures
    , ..
    })

printPkSigs
  :: Msig.Order
  -> ViaMultisigOptions
  -> TzTest ()
printPkSigs order vmo = do
  (_, (_, pkSigs)) <- signViaMultisig order vmo
  forM_ pkSigs $ \(pk, sig) ->
    putTextLn $ formatPublicKey pk <> ":" <> formatSignature sig

callViaMultisig
  :: Msig.Order
  -> ViaMultisigOptions
  -> TzTest ()
callViaMultisig order vmo@(ViaMultisigOptions {..}) = do
  fromAddr <- maybe (fail "From address not specified")
                    (Tz.resolve' Tz.AddressAlias) vmoFrom
  (msigAddr, (param, _)) <- signViaMultisig order vmo
  Tz.call fromAddr msigAddr param

handleFrozenMultisig
  :: Bool
  -> Tz.OrAlias L.Address
  -> STKR.PermitOnFrozenParam
  -> ViaMultisigOptions
  -> ReaderT Tz.Env IO ()
handleFrozenMultisig printSigs_ stkrAddrOrAlias stkrParam vmo = do
  stkrAddr <- Tz.resolve' Tz.ContractAlias stkrAddrOrAlias
  let order = Client.mkStkrFrozenOrder stkrParam stkrAddr
  bool callViaMultisig printPkSigs printSigs_ order vmo

handleOpsMultisig
  :: Bool
  -> Tz.OrAlias L.Address
  -> STKR.OpsTeamEntrypointParam
  -> ViaMultisigOptions
  -> ReaderT Tz.Env IO ()
handleOpsMultisig printSigs_ stkrAddrOrAlias stkrParam vmo = do
  stkrAddr <- Tz.resolve' Tz.ContractAlias stkrAddrOrAlias
  let order = Client.mkStkrOpsOrder stkrParam stkrAddr
  bool callViaMultisig printPkSigs printSigs_ order vmo

rotateKeys
  :: Bool
  -> Set L.KeyHash
  -> ViaMultisigOptions
  -> ReaderT Tz.Env IO ()
rotateKeys printSigs_ teamKeys vmo = do
  let order = Msig.mkRotateKeysOrder teamKeys
  bool callViaMultisig printPkSigs printSigs_ order vmo

remoteCmdRunner :: RemoteCommand -> TzTest ()
remoteCmdRunner = \case
  Deploy DeployOptions{..} -> do
    let msigKeyName i = msigAlias <> "_key_" <> show (i :: Int)
    teamKeys <-
      fmap Set.fromList $
        if null teamPkHashes
        then mapM (fmap hashKey . Tz.generateKey . msigKeyName) [1..3]
        else pure teamPkHashes
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
    handleOpsMultisig npPrintSigs npStkr (STKR.NewProposal prop) npViaMultisig
  NewCouncil NewCouncilOptions {..} -> do
    let genCouncil (prefix, n) =
          mapM (\i -> fmap hashKey . Tz.generateKey $ prefix <> "_key_" <> show i) [1..n]
    council <- either (fmap Set.fromList . genCouncil) pure ncCouncil
    handleOpsMultisig ncPrintSigs ncStkr (STKR.NewCouncil council) ncViaMultisig
  RotateMsigKeys RotateMsigKeysOptions {..} -> do
    rotateKeys nmPrintSigs (Set.fromList nmNewKeys) nmViaMultisig
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
    whose <-
      case (gbWhose, gbUseReservoir) of
        (Nothing, True) -> pure STKR.reservoirAddr
        (Just payer, False) -> Tz.resolve' Tz.AddressAlias payer
        _ -> fail "Either --reservoir or --addr <addr> should be specified"
    Client.getBalance stkrAddr whose
  GetTotalSupply GetTotalSupplyOptions {..} -> do
    -- fromAddr <- Tz.resolve' Tz.AddressAlias gbFrom -- use getStorage API ATM
    stkrAddr <- Tz.resolve' Tz.ContractAlias gtsStkr
    Client.getTotalSupply stkrAddr
  Transfer TransferOptions {..} -> do
    from <-
      case (tPayer, tUseReservoir) of
        (Nothing, True) -> pure STKR.reservoirAddr
        (Just payer, False) -> Tz.resolve' Tz.AddressAlias payer
        _ -> fail "Either --reservoir or --payer <addr> should be specified"
    to <- Tz.resolve' Tz.AddressAlias tReceiver
    let param = STKR.Transfer
                  ( #from .! from
                  , #to .! to
                  , #value .! tVal )
    handleOpsMultisig tPrintSigs tStkr param tViaMultisig
  Freeze FreezeOptions {..} ->
    handleOpsMultisig fPrintSigs fStkr (STKR.Freeze ()) fViaMultisig
  SetSuccessor SetSuccessorOptions {..} -> do
    newStkrAddr <- Tz.resolve' Tz.ContractAlias ssSucc
    let param = STKR.SetSuccessor $ #successor $
                  STKR.successorLambda (toContractRef newStkrAddr)
    handleFrozenMultisig ssPrintSigs ssStkr param ssViaMultisig
  Withdraw WithdrawOptions {..} -> do
    toAddr <- Tz.resolve' Tz.AddressAlias wReceiver
    -- FIXME??? We use `unsafeMkMutez` which throws instead of manual handling of `Nothing`
    let param = STKR.Withdraw (#to toAddr, #amount (unsafeMkMutez wAmount))
    handleFrozenMultisig wPrintSigs wStkr param wViaMultisig
