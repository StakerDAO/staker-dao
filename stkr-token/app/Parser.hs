module Parser
  ( CliCommand (..)
  , LocalCommand (..)
  , RemoteAction (..)
  , TzEnvConfig (..)
  , RemoteCommand (..)
  , DeployOptions (..)
  , NewProposalOptions (..)
  , NewCouncilOptions (..)
  , ViaMultisigOptions (..)
  , VoteForProposalOptions (..)
  , GetBalanceOptions (..)
  , GetTotalSupplyOptions (..)
  , TransferOptions (..)
  , SetSuccessorOptions (..)
  , WithdrawOptions (..)
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
  | GetBalance GetBalanceOptions
  | GetTotalSupply GetTotalSupplyOptions
  | Transfer TransferOptions
  | Freeze ViaMultisigOptions
  | SetSuccessor SetSuccessorOptions
  | Withdraw WithdrawOptions

data NewCouncilOptions = NewCouncilOptions
  { ncViaMultisig :: ViaMultisigOptions
  , ncCouncil :: Either (Text, Int) (Set KeyHash)
  }

data TransferOptions = TransferOptions
  { tViaMultisig :: ViaMultisigOptions
  , tFrom :: OrAlias Address
  , tTo :: OrAlias Address
  , tVal :: Natural
  }

data SetSuccessorOptions = SetSuccessorOptions
  { ssViaMultisig :: ViaMultisigOptions
  , ssSucc :: OrAlias Address
  }

data WithdrawOptions = WithdrawOptions
  { wViaMultisig :: ViaMultisigOptions
  , wFrom :: OrAlias Address
  , wAmount :: Word64
  }

data GetBalanceOptions = GetBalanceOptions
  { gbStkr :: OrAlias Address
  -- , gbFrom :: OrAlias Address -- use getStorage API ATM
  , gbWhose :: OrAlias Address
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
  }

data ViaMultisigOptions = ViaMultisigOptions
  { vmoMsig :: OrAlias Address
  , vmoStkr :: OrAlias Address
  , vmoFrom :: Maybe (OrAlias Address)
  , vmoMsigSignatures :: [OrAlias (PublicKey, Signature)]
  , vmoNonce :: Maybe Natural
  }

data NewProposalOptions = NewProposalOptions
  { npViaMultisig :: ViaMultisigOptions
  , npProposalFile :: FilePath
  , npPrintSigs :: Bool
  }

data DeployOptions = DeployOptions
  { msigAlias :: Text
  , stkrAlias :: Text
  , originator :: OrAlias Address
  , teamPksFiles :: [FilePath]
  , timeConfig :: TimeConfig
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
        , getBalanceSubprs
        , getTotalSupplySubprs
        , transferSubprs
        , freezeSubprs
        , setSuccessorSubprs
        , withdrawSubprs
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
         <*> many (fileArg)
         <*> timeConfigOption
        )

    newProposalSubprs = mkRemoteCmdPrs "new-proposal" "Submit new proposal" $
      NewProposal <$>
        (NewProposalOptions
          <$> viaMultisigOptions
          <*> proposalOption
          <*> printSigsOnlyOption
        )

    viaMultisigOptions = ViaMultisigOptions
      <$> addrOrAliasOption "msig"
      <*> addrOrAliasOption "stkr"
      <*> optional (addrOrAliasOption "from")
      <*> many (pkSigOption "msig")
      <*> nonceOption

    newCouncilSubprs = mkRemoteCmdPrs "new-council" "Rotate council keys" $
      NewCouncil <$>
        (NewCouncilOptions
          <$> viaMultisigOptions
          <*> councilOption
        )

    voteSubprs = mkRemoteCmdPrs "vote" "Vote for a proposal" $
      VoteForProposal <$>
        (VoteForProposalOptions
          <$> addrOrAliasOption "stkr"
          <*> addrOrAliasOption "from"
          <*> pkSigOption "member"
          <*> epochOption
          <*> proposalIdOption
        )

    printStorageSubprs = mkRemoteCmdPrs "print-storage" "Print storage of a contract" $
      PrintStorage <$> addrOrAliasOption "contract"

    getBalanceSubprs = mkRemoteCmdPrs "get-balance-stkr" "Get balance of a STKR token address" $
      GetBalance <$>
        (GetBalanceOptions
          <$> addrOrAliasOption "stkr"
          -- <*> addrOrAliasOption "from" -- use getStorage API ATM
          <*> addrOrAliasOption "whose"
        )

    getTotalSupplySubprs = mkRemoteCmdPrs "get-total-stkr" "Get total supply of STKR" $
      GetTotalSupply <$>
        (GetTotalSupplyOptions
          <$> addrOrAliasOption "stkr"
          -- <*> addrOrAliasOption "from" -- use getStorage API ATM
        )

    transferSubprs = mkRemoteCmdPrs "transfer" "Transfer tokens" $
      Transfer <$>
        (TransferOptions
          <$> viaMultisigOptions
          <*> addrOrAliasOption "from"
          <*> addrOrAliasOption "to"
          <*> valueOption
        )

    freezeSubprs = mkRemoteCmdPrs "freeze" "Freeze the contract" $
      Freeze <$> viaMultisigOptions

    setSuccessorSubprs = mkRemoteCmdPrs "set-successor-stkr" "Set the successor of STKR contract" $
      SetSuccessor <$>
        (SetSuccessorOptions
          <$> viaMultisigOptions
          <*> addrOrAliasOption "succ"
        )

    withdrawSubprs = mkRemoteCmdPrs "withdraw" "Withdraw funds from frozen STKR contract" $
      Withdraw <$>
        (WithdrawOptions
          <$> viaMultisigOptions
          <*> addrOrAliasOption "from"
          <*> amountOption
        )
