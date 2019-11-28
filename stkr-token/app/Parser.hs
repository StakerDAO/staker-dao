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

import qualified TzTest as Tz

import Options

data CliCommand
  = Local LocalCommand
  | Remote RemoteAction

data LocalCommand
  = PrintContract (Maybe FilePath)

data RemoteAction = RemoteAction
  { tzEnvConfig :: TzEnvConfig
  , remoteCmd ::RemoteCommand
  }

data TzEnvConfig
  = YamlFile FilePath
  | CliArgs Tz.Env

data RemoteCommand
  = Deploy DeployOptions
  | PrintStorage Address

data DeployOptions = DeployOptions
  { contractAlias :: Text
  , originator :: Address
  , teamPksFiles :: [FilePath]
  }


mkCmdPrs
  :: String
  -> String
  -> Opt.Parser CliCommand
  -> Opt.Mod Opt.CommandFields CliCommand
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
    Remote <$> (RemoteAction <$> (envPrs <|> configPrs) <*> prs)
  where
    envPrs =
      CliArgs <$>
      (Tz.Env
      <$> (Opt.strOption $ Opt.long "tzclient")
      <*> (Opt.strOption $ Opt.short 'A')
      <*> (Opt.option (Opt.auto @Natural) $ Opt.short 'P')
      )

    configPrs =
      YamlFile <$>
      (Opt.strOption $ mconcat
        [ Opt.short 'c'
        , Opt.long "config"
        ])

exeDesc :: [Char]
exeDesc =
  "This client allows one to deploy STKR and STKRC"
  <> " contracts on a Tezos network."

deployDesc :: [Char]
deployDesc =
  "Deploy contract to Tezos network with supplied set of team keys "
  <> "(each key is provided as standalone PK file)."

cmdParser :: Opt.ParserInfo (CliCommand)
cmdParser = info (helper <*> subparsers) (progDesc exeDesc)
  where
    subparsers = Opt.subparser . mconcat $
        [ printSubprs
        , deploySubprs
        , printStorageSubprs
        ]

    printSubprs = mkCmdPrs "printContract" "Print contract to stdout" $
      Local . PrintContract <$> fileOutputOption

    deploySubprs = mkRemoteCmdPrs "deploy" deployDesc $
      Deploy <$>
        (DeployOptions
         <$> contractAliasOption
         <*> addressOption "from"
         <*> many (fileArg)
        )

    printStorageSubprs = mkRemoteCmdPrs "printStorage" "Print storage of a contract" $
      PrintStorage <$> addressArg
