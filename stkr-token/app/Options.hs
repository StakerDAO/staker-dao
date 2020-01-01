module Options
  ( fileOutputOption
  , aliasOption
  , fileArg

  , OrAlias (..)
  , addrOrAliasOption

  , addressOption

  , TzEnvConfig (..)
  , tzEnvOptions

  , pkSigOption

  , startOption
  , durationOption
  , startYearOption

  , proposalOption
  , nonceOption
  , timeConfigOption
  , councilOption

  , proposalIdOption
  , epochOption

  , valueOption
  , amountOption
  , printSigsOnlyOption
  ) where

import Prelude

import Data.Word (Word64) -- FIXME!!! derive Read for Mutez in Morley
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Options.Applicative as Opt
import Fmt (pretty)

import Tezos.Address (Address, parseAddress)
import Tezos.Core (Timestamp, timestampFromSeconds)
import Tezos.Crypto (KeyHash, PublicKey, Signature, parsePublicKey, parseSignature, parseKeyHash)

import qualified TzTest as Tz
import TzTest (OrAlias)

import Lorentz.Contracts.STKR (TimeConfig (..))

fileOutputOption :: Opt.Parser (Maybe FilePath)
fileOutputOption = Opt.optional $ Opt.strOption $ mconcat
  [ Opt.short 'o'
  , Opt.long "output"
  , Opt.metavar "FILEPATH"
  , Opt.help "Output file"
  ]

aliasOption :: String -> Opt.Parser Text
aliasOption name = Opt.strOption $ mconcat
  [ Opt.long $ name <> "Alias"
  , Opt.metavar "ALIAS"
  ]

addrOrAliasOption :: String -> Opt.Parser (OrAlias Address)
addrOrAliasOption name =
  (Tz.Alias <$> aliasOption name)
    <|> (Tz.Value <$> addressOption name)

addressOption :: String -> Opt.Parser Address
addressOption name =
  Opt.option addressReader $
    mconcat [Opt.long name, Opt.metavar "ADDRESS"]

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

keyHashReader :: Opt.ReadM KeyHash
keyHashReader = Opt.eitherReader $ \addr ->
   either
        (Left . mappend "Failed to parse key hash: " . pretty)
        Right $
        parseKeyHash $ toText addr

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

pkSigOption :: String -> Opt.Parser (OrAlias (PublicKey, Signature))
pkSigOption prefix = pkSig <|> (fmap Tz.Alias $ aliasOption $ prefix <> "Key")
  where
    pkSig = Tz.Value <$> Opt.option pkSigReader pkSigOpts
    pkSigOpts = mconcat [ Opt.long $ prefix <> "Sig"
                        , Opt.metavar "PublicKey:Signature" ]

pkSigReader :: Opt.ReadM (PublicKey, Signature)
pkSigReader = Opt.eitherReader $ \txt -> do
  (pkTxt, sigTxt) <-
    case T.splitOn ":" (T.pack txt) of
      pt:st:[] -> pure (pt, st)
      _ -> Left "Expected format: \"<pk>:<sig>\""
  pk <- handleErr "public key" (parsePublicKey pkTxt)
  sig <- handleErr "signature" (parseSignature sigTxt)
  pure (pk, sig)
  where
    handleErr subj =
      either (Left . (<>) ("Failed to parse "
                              <>subj<>": ") . pretty) Right

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

proposalOption :: Opt.Parser FilePath
proposalOption =
  (Opt.strOption $ mconcat
    [ Opt.short 'p'
    , Opt.long "proposal-file"
    , Opt.metavar "PROPOSAL YAML_FILE"
    ])

nonceOption :: Opt.Parser (Maybe Natural)
nonceOption = Opt.optional $ Opt.option Opt.auto (Opt.long "nonce")

timeConfigOption :: Opt.Parser TimeConfig
timeConfigOption = test <|> prod
  where
    prod =
      const ProdTC
        <$> Opt.switch (Opt.long "prod" <> Opt.help "Run in production mode")
        <*> startYearOption
    test =
      const TestTC
        <$> Opt.switch (Opt.long "test" <> Opt.help "Run in test mode")
        <*> startOption <*> durationOption

councilOption :: Opt.Parser (Either (Text, Int) (Set KeyHash))
councilOption = Left <$> genKeys <|> Right <$> readKeys
  where
    readKeys =
      fmap S.fromList $ many $
      Opt.option keyHashReader $
        Opt.long "member" <> Opt.metavar "KEY_HASH"
          <> Opt.metavar "Hash of council member's key"
    genKeys =
      (,)
      <$>
      Opt.strOption
        ( Opt.long "prefix" <> Opt.metavar "STRING"
          <> Opt.help "Prefix for generated council keys" )
      <*>
      Opt.option Opt.auto
        ( Opt.short 'n' <> Opt.long "councilSize" <> Opt.metavar "INT"
          <> Opt.help "Size of a council" )

proposalIdOption :: Opt.Parser Natural
proposalIdOption = Opt.option Opt.auto $
  Opt.long "proposal"
    <> Opt.short 'p'
    <> Opt.help "Id of proposal to vote for"
    <> Opt.metavar "INT"

epochOption :: Opt.Parser Natural
epochOption = Opt.option Opt.auto $
  Opt.long "epoch" <> Opt.short 'e'
    <> Opt.help "Epoch in which a proposal to vote was created"
    <> Opt.metavar "INT"

valueOption :: Opt.Parser Natural
valueOption = Opt.option Opt.auto $
  Opt.long "value" <> Opt.short 'v'
    <> Opt.help "Value to transfer"
    <> Opt.metavar "INT"

amountOption :: Opt.Parser Word64
amountOption = Opt.option Opt.auto $
  Opt.long "amount" <> Opt.short 'a'
    <> Opt.help "Amount to withdraw"
    <> Opt.metavar "INT"

printSigsOnlyOption :: Opt.Parser Bool
printSigsOnlyOption = Opt.switch $
  Opt.long "print-sigs"
    <> Opt.help "Print signatures only, do not submit proposal to network"
