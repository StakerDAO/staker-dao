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
  ) where

import Prelude

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Options.Applicative as Opt
import Text.Hex (decodeHex)
import Fmt (pretty)

import Tezos.Address (Address, parseAddress)
import Michelson.Text (MText, mkMText)
import Tezos.Core (Timestamp, timestampFromSeconds)
import Tezos.Crypto (KeyHash, PublicKey, Signature, parsePublicKey, parseSignature, parseKeyHash)

import qualified TzTest as Tz
import TzTest (OrAlias)

import Lorentz.Contracts.STKR (Hash, Proposal, TimeConfig (..), URL)

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

urlDesc :: String
urlDesc =
  "An URL to be stored in contract.\n"
  <>
  "Colon-separated triple should be given: a name of an URL, "
  <>
  "SHA256 hash of the document stored at URL "
  <>
  "(in hexadecimal format) and URL itself."

proposalOption :: Opt.Parser Proposal
proposalOption =
  (\desc plc -> (#description desc, #newPolicy plc)) <$>
    descOption <*> (many urlOption <&> #urls . M.fromList)
  where
    descOption = Opt.option (Opt.eitherReader mkMText')
                  ( Opt.long "desc" <> Opt.metavar "TEXT"
                    <> Opt.help "Description of the proposal" )
    urlOption = Opt.option urlReader
                  ( Opt.long "url" <> Opt.metavar "NAME:HASH:URL"
                    <> Opt.help urlDesc)
    mkMText' = either (Left . ("Failed to parse desc: " <>) . T.unpack)
                      Right . mkMText . T.pack

urlReader :: Opt.ReadM (MText, (Hash, URL))
urlReader = Opt.eitherReader $ \txt -> do
  (nameTxt, hashTxt, urlTxt) <-
    case T.splitOn ":" (T.pack txt) of
      n:h:u -> pure (n, h, T.intercalate ":" u)
      _ -> Left "Expected format: \"<name>:<hash>:<url>\""
  name <- handleErr "name" (mkMText nameTxt)
  hash <- handleErr "hash" $ maybe (Left ("wrong" :: Text)) pure
                                (decodeHex hashTxt)
  url <- handleErr "URL" (mkMText urlTxt)
  pure (name, (hash, url))
  where
    handleErr subj =
      either (Left . (<>) ("Failed to parse "
                              <>subj<>": ") . pretty) Right

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
