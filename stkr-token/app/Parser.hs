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
  , cmdParser
  ) where

import Prelude

import Options.Applicative (command, helper, info, progDesc)
import qualified Options.Applicative as Opt

import Tezos.Address (Address)
import Tezos.Crypto (KeyHash, PublicKey, Signature)

import Lorentz.Contracts.STKR (TimeConfig (..), Proposal)

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

data NewCouncilOptions = NewCouncilOptions
  { ncViaMultisig :: ViaMultisigOptions
  , ncCouncil :: Either (Text, Int) (Set KeyHash)
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
  , vmoFrom :: OrAlias Address
  , vmoMsigSignatures :: [OrAlias (PublicKey, Signature)]
  , vmoNonce :: Maybe Natural
  }

data NewProposalOptions = NewProposalOptions
  { npViaMultisig :: ViaMultisigOptions
  , npProposal :: Proposal
  }

data DeployOptions = DeployOptions
  { msigAlias :: Text
  , tokenAlias :: Text
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
        )

    viaMultisigOptions = ViaMultisigOptions
      <$> addrOrAliasOption "msig"
      <*> addrOrAliasOption "stkr"
      <*> addrOrAliasOption "from"
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
