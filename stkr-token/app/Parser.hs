module Parser
  ( CliCommand (..)
  , LocalCommand (..)
  , RemoteAction (..)
  , TzEnvConfig (..)
  , RemoteCommand (..)
  , DeployOptions (..)
  , NewProposalOptions (..)
  , NewCouncilOptions (..)
  , RotateMsigKeysOptions (..)
  , ViaMultisigOptions (..)
  , VoteForProposalOptions (..)
  , FundOptions (..)
  , GetBalanceOptions (..)
  , GetTotalSupplyOptions (..)
  , TransferOptions (..)
  , SetSuccessorOptions (..)
  , WithdrawOptions (..)
  , FreezeOptions (..)
  , cmdParser
  ) where

import Prelude

import Data.Word (Word64)
import Options.Applicative (command, helper, info, progDesc, optional)
import qualified Options.Applicative as Opt

import Tezos.Address (Address)
import Tezos.Crypto (KeyHash, PublicKey, Signature)

import Lorentz.Contracts.STKR (TimeConfig (..))

import Options

data CliCommand
  = Local LocalCommand
  | Remote RemoteAction

data LocalCommand
  = PrintStkr (Maybe FilePath) TimeConfig
  | PrintMultisig (Maybe FilePath)

data RemoteAction = RemoteAction
  { tzEnvConfig :: TzEnvConfig
  , remoteCmd :: RemoteCommand
  }

data RemoteCommand
  = Deploy DeployOptions
  | PrintStorage (OrAlias Address)
  | NewProposal NewProposalOptions
  | NewCouncil NewCouncilOptions
  | VoteForProposal VoteForProposalOptions
  | Fund FundOptions
  | GetBalance GetBalanceOptions
  | GetTotalSupply GetTotalSupplyOptions
  | Transfer TransferOptions
  | Freeze FreezeOptions
  | SetSuccessor SetSuccessorOptions
  | Withdraw WithdrawOptions
  | RotateMsigKeys RotateMsigKeysOptions

data RotateMsigKeysOptions = RotateMsigKeysOptions
  { nmViaMultisig :: ViaMultisigOptions
  , nmNewKeys :: [KeyHash]
  , nmPrintSigs :: Bool
  }

data NewCouncilOptions = NewCouncilOptions
  { ncViaMultisig :: ViaMultisigOptions
  , ncStkr :: OrAlias Address
  , ncCouncil :: Either (Text, Int) (Set KeyHash)
  , ncPrintSigs :: Bool
  }

data TransferOptions = TransferOptions
  { tViaMultisig :: ViaMultisigOptions
  , tStkr :: OrAlias Address
  , tPrintSigs :: Bool
  , tPayer :: Maybe (OrAlias Address)
  , tUseReservoir :: Bool
  , tReceiver :: OrAlias Address
  , tVal :: Natural
  }

data FreezeOptions = FreezeOptions
  { fViaMultisig :: ViaMultisigOptions
  , fStkr :: OrAlias Address
  , fPrintSigs :: Bool
  }

data SetSuccessorOptions = SetSuccessorOptions
  { ssViaMultisig :: ViaMultisigOptions
  , ssStkr :: OrAlias Address
  , ssPrintSigs :: Bool
  , ssSucc :: OrAlias Address
  }

data WithdrawOptions = WithdrawOptions
  { wViaMultisig :: ViaMultisigOptions
  , wStkr :: OrAlias Address
  , wPrintSigs :: Bool
  , wReceiver :: OrAlias Address
  , wAmount :: Word64
  }

data GetBalanceOptions = GetBalanceOptions
  { gbStkr :: OrAlias Address
  -- , gbFrom :: OrAlias Address -- use getStorage API ATM
  , gbWhose :: Maybe (OrAlias Address)
  , gbUseReservoir :: Bool
  }

data GetTotalSupplyOptions = GetTotalSupplyOptions
  { gtsStkr :: OrAlias Address
  -- , gtsFrom :: OrAlias Address -- use getStorage API ATM
  }

data VoteForProposalOptions = VoteForProposalOptions
  { vpStkr :: OrAlias Address
  , vpFrom :: OrAlias Address
  , vpPkSig :: OrAlias (PublicKey, Signature)
  , vpEpoch :: Natural
  , vpProposalId :: Natural
  , vpPrintSig :: Bool
  }

data FundOptions = FundOptions
  { fnStkr :: OrAlias Address
  , fnFrom :: OrAlias Address
  , fnPayload :: Text
  }

data ViaMultisigOptions = ViaMultisigOptions
  { vmoMsig :: OrAlias Address
  , vmoFrom :: Maybe (OrAlias Address)
  , vmoMsigSignatures :: [OrAlias (PublicKey, Signature)]
  , vmoNonce :: Maybe Natural
  }

data NewProposalOptions = NewProposalOptions
  { npViaMultisig :: ViaMultisigOptions
  , npStkr :: OrAlias Address
  , npProposalFile :: FilePath
  , npPrintSigs :: Bool
  }

data DeployOptions = DeployOptions
  { msigAlias :: Text
  , stkrAlias :: Text
  , originator :: OrAlias Address
  , teamPkHashes :: [KeyHash]
  , timeConfig :: TimeConfig
  , totalSupply_ :: Natural
  }

mkCmdPrs
  :: String
  -> String
  -> Opt.Parser a
  -> Opt.Mod Opt.CommandFields a
mkCmdPrs name desc prs =
  command name $
  info (helper <*> prs) $
  progDesc desc

mkRemoteCmdPrs
  :: String
  -> String
  -> Opt.Parser RemoteCommand
  -> Opt.Mod Opt.CommandFields CliCommand
mkRemoteCmdPrs name desc prs =
  mkCmdPrs name desc $
    Remote <$> (RemoteAction <$> tzEnvOptions <*> prs)

exeDesc :: [Char]
exeDesc =
  "This client allows one to deploy STKR and STKRC"
  <> " contracts on a Tezos network."

deployDesc :: [Char]
deployDesc =
  "Deploy contract to Tezos network with supplied set of team keys "
  <> "(each key is provided as standalone PK file)."

cmdParser :: Opt.ParserInfo CliCommand
cmdParser = info (helper <*> cmdImpl) (progDesc exeDesc)
  where
    cmdImpl :: Opt.Parser CliCommand
    cmdImpl = Opt.subparser . mconcat $
        [ printMsigSubprs
        , printStkrSubprs
        , deploySubprs
        , printStorageSubprs
        , newProposalSubprs
        , newCouncilSubprs
        , voteSubprs
        , fundSubprs
        , getBalanceSubprs
        , getTotalSupplySubprs
        , transferSubprs
        , freezeSubprs
        , setSuccessorSubprs
        , withdrawSubprs
        , newMsigKeysSubprs
        ]

    printMsigSubprs = mkCmdPrs "print-multisig" "Print multisig contract" $
      Local <$> (PrintMultisig <$> fileOutputOption)

    printStkrSubprs = mkCmdPrs "print-stkr" "Print STKR contract" $
      Local <$> (PrintStkr <$> fileOutputOption <*> timeConfigOption)

    deploySubprs = mkRemoteCmdPrs "deploy" deployDesc $
      Deploy <$>
        (DeployOptions
         <$> aliasOption "msig"
         <*> aliasOption "stkr"
         <*> addrOrAliasOption "from"
         <*> many pkHashOption
         <*> timeConfigOption
         <*> totalSupplyOption
        )

    newMsigKeysSubprs = mkRemoteCmdPrs "rotate-msig-keys" "Change keys of Msig contract" $
      RotateMsigKeys <$>
        (RotateMsigKeysOptions
          <$> viaMultisigOptions
          <*> many pkHashOption
          <*> printSigsOnlyOption
        )

    newProposalSubprs = mkRemoteCmdPrs "new-proposal" "Submit new proposal" $
      NewProposal <$>
        (NewProposalOptions
          <$> viaMultisigOptions
          <*> addrOrAliasOption "stkr"
          <*> proposalOption
          <*> printSigsOnlyOption
        )

    viaMultisigOptions = ViaMultisigOptions
      <$> addrOrAliasOption "msig"
      <*> optional (addrOrAliasOption "from")
      <*> many (pkSigOption "msig")
      <*> nonceOption

    newCouncilSubprs = mkRemoteCmdPrs "new-council" "Rotate council keys" $
      NewCouncil <$>
        (NewCouncilOptions
          <$> viaMultisigOptions
          <*> addrOrAliasOption "stkr"
          <*> councilOption
          <*> printSigsOnlyOption
        )

    voteSubprs = mkRemoteCmdPrs "vote" "Vote for a proposal" $
      VoteForProposal <$>
        (VoteForProposalOptions
          <$> addrOrAliasOption "stkr"
          <*> addrOrAliasOption "from"
          <*> pkSigOption "member"
          <*> epochOption
          <*> proposalIdOption
          <*> printSigsOnlyOption
        )

    fundSubprs = mkRemoteCmdPrs "fund" "Fund the contract" $
      Fund <$>
        (FundOptions
          <$> addrOrAliasOption "stkr"
          <*> addrOrAliasOption "from"
          <*> payloadOption "payload"
        )

    printStorageSubprs = mkRemoteCmdPrs "print-storage" "Print storage of a contract" $
      PrintStorage <$> addrOrAliasOption "contract"

    getBalanceSubprs = mkRemoteCmdPrs "get-balance" "Get balance of a STKR token address" $
      GetBalance <$>
        (GetBalanceOptions
          <$> addrOrAliasOption "stkr"
          -- <*> addrOrAliasOption "from" -- use getStorage API ATM
          <*> optional (addrOrAliasOption "addr")
          <*> reservoirOption
        )

    getTotalSupplySubprs = mkRemoteCmdPrs "total-supply" "Get total supply of STKR" $
      GetTotalSupply <$>
        (GetTotalSupplyOptions
          <$> addrOrAliasOption "stkr"
          -- <*> addrOrAliasOption "from" -- use getStorage API ATM
        )

    transferSubprs = mkRemoteCmdPrs "transfer" "Transfer tokens" $
      Transfer <$>
        (TransferOptions
          <$> viaMultisigOptions
          <*> addrOrAliasOption "stkr"
          <*> printSigsOnlyOption
          <*> optional (addrOrAliasOption "payer")
          <*> reservoirOption
          <*> addrOrAliasOption "receiver"
          <*> valueOption
        )

    freezeSubprs = mkRemoteCmdPrs "freeze" "Freeze the contract" $
      Freeze <$>
        (FreezeOptions
          <$> viaMultisigOptions
          <*> addrOrAliasOption "stkr"
          <*> printSigsOnlyOption
        )

    setSuccessorSubprs = mkRemoteCmdPrs "set-successor" "Set the successor of STKR contract" $
      SetSuccessor <$>
        (SetSuccessorOptions
          <$> viaMultisigOptions
          <*> addrOrAliasOption "stkr"
          <*> printSigsOnlyOption
          <*> addrOrAliasOption "succ"
        )

    withdrawSubprs = mkRemoteCmdPrs "withdraw" "Withdraw funds from STKR contract" $
      Withdraw <$>
        (WithdrawOptions
          <$> viaMultisigOptions
          <*> addrOrAliasOption "stkr"
          <*> printSigsOnlyOption
          <*> addrOrAliasOption "receiver"
          <*> amountOption
        )
