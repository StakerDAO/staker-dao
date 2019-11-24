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
  { contractName :: Text
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



cmdParser :: Opt.ParserInfo (CliCommand)
cmdParser = info (helper <*> subparsers) (progDesc "Desc")
  where
    subparsers = Opt.subparser . mconcat $
        [ printSubprs
        , deploySubprs
        , printStorageSubprs
        ]

    printSubprs = mkCmdPrs "printContract" "print description" $
      Local . PrintContract <$> fileOutputOption

    deploySubprs = mkRemoteCmdPrs "deploy" "deploy desc" $
      Deploy <$>
        (DeployOptions
         <$> contractNameOption
         <*> addressOption "from"
         <*> many (fileArg)
        )

    printStorageSubprs = mkRemoteCmdPrs "printStorage" "Desc" $
      PrintStorage <$> addressArg
