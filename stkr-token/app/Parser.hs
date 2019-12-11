module Parser
  ( CliCommand(..)
  , LocalCommand(..)
  , RemoteAction(..)
  , TzEnvConfig(..)
  , RemoteCommand(..)
  , DeployOptions(..)
  , cmdParser
  ) where

import Prelude

import Options.Applicative (command, helper, info, progDesc)
import qualified Options.Applicative as Opt

import Tezos.Address (Address)

import Lorentz.Contracts.STKR (TimeConfig (..))

import Options

data CliCommand
  = Local LocalCommand
  | Remote RemoteAction

data LocalCommand
  = PrintStkr (Maybe FilePath)
  | PrintMultisig (Maybe FilePath)

data RemoteAction = RemoteAction
  { tzEnvConfig :: TzEnvConfig
  , remoteCmd ::RemoteCommand
  }

data RemoteCommand
  = Deploy DeployOptions
  | PrintStorage Address

data DeployOptions = DeployOptions
  { msigAlias :: Text
  , tokenAlias :: Text
  , originator :: Address
  , teamPksFiles :: [FilePath]
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

cmdParser :: Opt.ParserInfo (CliCommand, TimeConfig)
cmdParser = info (helper <*> toplevel) (progDesc exeDesc)
  where
    toplevel = (,) <$> cmdImpl <*> tcImpl

    cmdImpl :: Opt.Parser CliCommand
    cmdImpl = Opt.subparser . mconcat $
        [ printMsigSubprs
        , printStkrSubprs
        , deploySubprs
        , printStorageSubprs
        ]

    tcImpl :: Opt.Parser TimeConfig
    tcImpl = Opt.subparser (test <> prod)

    test = mkCmdPrs "prod" "Run in production mode" $
              ProdTC <$> startYearOption
    prod = mkCmdPrs "test" "Run in test mode" $
              TestTC <$> startOption <*> durationOption

    printMsigSubprs = mkCmdPrs "printMultisig" "Print multisig contract" $
      Local <$> (PrintMultisig <$> fileOutputOption)

    printStkrSubprs = mkCmdPrs "printStkr" "Print STKR contract" $
      Local <$> (PrintStkr <$> fileOutputOption)

    deploySubprs = mkRemoteCmdPrs "deploy" deployDesc $
      Deploy <$>
        (DeployOptions
         <$> contractAliasOption "msig"
         <*> contractAliasOption "stkr"
         <*> addressOption "from"
         <*> many (fileArg)
        )

    printStorageSubprs = mkRemoteCmdPrs "printStorage" "Print storage of a contract" $
      PrintStorage <$> addressArg
