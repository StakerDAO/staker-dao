module Options
  ( fileOutputOption
  , contractAliasOption
  , fileArg

  , addressOption
  , addressArg
  ) where

import Prelude

import Data.Text as T
import qualified Options.Applicative as Opt

import Fmt (pretty)
import Tezos.Address (Address, parseAddress)
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
fileArg = Opt.strArgument $ mconcat
  [ Opt.metavar "FILEPATH"
  ]

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

tzEnvOptions :: Opt.Parser (Maybe FilePath)
tzEnvOptions = Opt.optional configFileOption
  where
    configFileOption = Opt.strOption $ mconcat
      [ Opt.short 'c'
      , Opt.long "config"
      , Opt.metavar "FILEPATH"
      ]
