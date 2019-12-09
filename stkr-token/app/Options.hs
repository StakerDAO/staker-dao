module Options
  ( fileOutputOption
  , contractAliasOption
  , fileArg

  , addressOption
  , addressArg

  , TzEnvConfig (..)
  , tzEnvOptions

  , startOption
  , durationOption
  , startYearOption
  ) where

import Prelude

import Data.Text as T
import qualified Options.Applicative as Opt

import Fmt (pretty)
import Tezos.Address (Address, parseAddress)
import Tezos.Core (Timestamp, timestampFromSeconds)

import qualified TzTest as Tz

fileOutputOption :: Opt.Parser (Maybe FilePath)
fileOutputOption = Opt.optional $ Opt.strOption $ mconcat
  [ Opt.short 'o'
  , Opt.long "output"
  , Opt.metavar "FILEPATH"
  , Opt.help "Output file"
  ]

contractAliasOption :: Text -> Opt.Parser Text
contractAliasOption name = Opt.strOption $ mconcat
  [ Opt.long . T.unpack $ name <> "Alias"
  , Opt.metavar "NAME"
  ]

addressOption :: String -> Opt.Parser Address
addressOption long = Opt.option addressReader $ mconcat [Opt.long long, Opt.metavar "ADDRESS"]

fileArg :: Opt.Parser FilePath
fileArg = Opt.strOption $ mconcat
  [ Opt.metavar "PK FILE"
  , Opt.long "team-pk"
  ]

addressReader :: Opt.ReadM Address
addressReader = Opt.eitherReader $ \addr ->
   either
        (Left . mappend "Failed to parse address: " . pretty)
        Right $
        parseAddress $ toText addr

addressArg :: Opt.Parser Address
addressArg =
  Opt.argument (Opt.eitherReader parseAddress') $
  Opt.metavar "ADDRESS"
  where
    parseAddress' addr =
      either
        (Left . mappend "Failed to parse address: " . pretty)
        Right $
        parseAddress $ toText addr

data TzEnvConfig
  = YamlFile FilePath
  | CliArgs Tz.Env

tzEnvOptions :: Opt.Parser TzEnvConfig
tzEnvOptions = envPrs <|> configPrs
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
        , Opt.metavar "YAML_FILE"
        ])

durationOption :: Opt.Parser Natural
durationOption =
  Opt.option Opt.auto
    ( Opt.long "duration" <> Opt.metavar "SECONDS"
      <> Opt.help "Duration of a stage (for test mode)"
    )

startYearOption :: Opt.Parser Natural
startYearOption =
  Opt.option Opt.auto
    ( Opt.long "start-year" <> Opt.metavar "YEAR"
      <> Opt.help "Year of first epoch (for prod mode)"
    )

startOption :: Opt.Parser Timestamp
startOption =
  fmap timestampFromSeconds $
    Opt.option Opt.auto
      ( Opt.long "start" <> Opt.metavar "TIMESTAMP"
        <> Opt.help "Time of first epoch start (for test mode)"
      )
