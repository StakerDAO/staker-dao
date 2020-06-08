{-# LANGUAGE NoRebindableSyntax #-}

module Client.Tezos.Operations
 ( TransferP(..)
  , transfer
  , call

  , OriginateContractP(..)
  , originateContract

  , generateKey
  , importSecretKey
  , getStorage

  , getChainId
  , getMainChainId
  , getHeadTimestamp
  , getElementTextOfBigMapByAddress

  , resolve
  , resolve'
  , ResolveType (..)
  , OrAlias (..)
  ) where

import Prelude hiding (hPutStrLn, unlines)

import Data.Singletons (SingI)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Fmt (Buildable, pretty)
import Lens.Micro (ix)
import Text.Hex (encodeHex)

import qualified Crypto.Error as CE
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Lorentz
  (ContractCode, NiceParameterFull, NicePrintedValue, NiceStorage, lPackValue,
  parseLorentzValue)
import Lorentz.Print (printLorentzContract, printLorentzValue)
import Michelson.Typed (IsoValue, ToT)
import Tezos.Address (Address, formatAddress, parseAddress)
import Tezos.Core
  (ChainId, Mutez(..), Timestamp, parseChainId, parseTimestamp, unsafeMkMutez)
import qualified Tezos.Crypto.Ed25519 as TzEd25519


import Data.Maybe (fromJust)
import System.Console.Haskeline (defaultSettings, getPassword, runInputT)

import Client.DecipherTzEncKey
import Lorentz.CryptoInterop
  (KeyHash, PublicKey(..), SecretKey, Signature(..), blake2b,
  encodeBase58Check, formatSecretKey, parseKeyHash, parsePublicKey,
  parseSecretKey, parseSignature, sign)

import Client.Tezos.Core
  (TzEnv, Verbosity(..), exec, execSilent, execSilentWithStdout)


data TransferP a = TransferP
  { tpQty :: Mutez
  , tpSrc :: Address
  , tpDst :: Address
  , tpBurnCap :: Natural
  , tpArgument :: a
  }

transfer
  :: NicePrintedValue a
  => TransferP a -> TzEnv ()
transfer TransferP{..} = void $ do
  exec ShowBoth $
    [ "transfer", toText mtzShown
    , "from", formatAddress tpSrc
    , "to", formatAddress tpDst
    , "--burn-cap", show tpBurnCap
    , "--arg", toStrict $ printLorentzValue True tpArgument
    ]
  where
    mtz = unMutez tpQty
    mtzShown = show (mtz `div` 1000000) <> "." <> mtzRemShown
    mtzRemShown' = show (mtz `mod` 1000000)
    mtzRemShown = replicate (6 - length mtzRemShown') '0' <> mtzRemShown'

call
  :: NicePrintedValue p
  => Address
  -> Address
  -> p
  -> TzEnv ()
call caller contract parameter = transfer $
  TransferP
    { tpQty = unsafeMkMutez 0
    , tpSrc = caller
    , tpDst = contract
    , tpBurnCap = 22
    , tpArgument = parameter
    }

data OriginateContractP p st =
  OriginateContractP
  { ocpAlias :: Text
  , ocpQty :: Natural
  , ocpSrc :: Address
  , ocpContract :: ContractCode p st
  , ocpInitalStorage :: st
  , ocpBurnCap :: Natural
  }

originateContract
  :: forall p st.
  ( NiceStorage st
  , NiceParameterFull p
  )
  => OriginateContractP p st -> TzEnv Address
originateContract OriginateContractP{..} = do
  let contractString = toStrict . printLorentzContract True $ ocpContract
  let initString = toStrict . printLorentzValue True $ ocpInitalStorage
  let cmdArgs =
        [ "originate", "contract", ocpAlias
        , "transferring", show ocpQty
        , "from", formatAddress ocpSrc
        , "running", contractString
        , "--init", initString
        , "--burn-cap", show ocpBurnCap
        ]
  addrString <- fromMaybe "" . safeHead . words . T.strip .
                -- we show tezos-client output since it may ask
                -- to supply passwords for encrypted keys
                lineWithPrefix "New contract " . fst <$> exec ShowBoth cmdArgs
  -- Ex: New contract KT1MNzB6r9eFiYtFbhnRUgnuC83vwSUqERWG originated.
  either (fail . pretty) pure $ parseAddress addrString

data OrAlias a = Alias Text | Value a

data ResolveType t where
  PublicKeyAlias :: ResolveType PublicKey
  KeyHashAlias :: ResolveType KeyHash
  ContractAlias :: ResolveType Address
  AddressAlias :: ResolveType Address
  PkSigAlias :: ByteString -> ResolveType (PublicKey, Signature)

resolve'
  :: ResolveType t -> OrAlias t -> TzEnv t
resolve' _ (Value v) = pure v
resolve' rt (Alias al) = resolve rt al

lineWithPrefix :: Text -> Text -> Text
lineWithPrefix prefix txt
  | T.null prefix = txt
  | otherwise =
      fromMaybe "" . safeHead $
      mapMaybe (T.stripPrefix prefix) (lines txt)

-- Tezos.Crypto exports SecretKey abstractly, don't bother to change it
mySignPK :: ByteString -> ByteString -> Signature
mySignPK skBytes bytes = SignatureEd25519 . TzEd25519.Signature . Ed25519.sign sk pk . blake2b $ bytes
  where
    sk = CE.throwCryptoError $ Ed25519.secretKey skBytes
    pk = Ed25519.toPublic sk


signWithTezosClient :: Text -> ByteString -> TzEnv Signature
signWithTezosClient accountName bytes = do
  let hexString = encodeHex bytes
  signatureString <- lineWithPrefix "Signature: " . fst <$>
    exec ShowStdErr
      [ "sign", "bytes", "0x" <> hexString
      , "for", accountName]
  either ( fail . ("Signature parsing failed: " <>) . pretty) pure $ parseSignature signatureString

generateSigPK
  :: Text -> ByteString -> TzEnv (PublicKey, Signature)
generateSigPK alias toSign = do
  cmdOut <- execSilentWithStdout ["show","address", alias, "-S"]
  let pkString = lineWithPrefix "Public Key: " cmdOut
  pk <- either (fail . pretty) pure $ parsePublicKey pkString
  let tzsk = lineWithPrefix "Secret Key: " cmdOut
  -- tzsk is in format keyType:publicKey
  let (keyType, left) = T.breakOn ":" tzsk
  let key = T.tail left -- TODO proper parsing and error handling
  let chosenMethod =
        case keyType of
          "ledger" -> signWithLedger alias
          "encrypted" -> signEncrypted key
          "unencrypted" -> signUnencrypted key
          _ -> const $ fail "Error on secret key lookup, secret key type not recognized"
  signature <- chosenMethod toSign
  pure (pk, signature)
  where
    signWithLedger :: Text -> ByteString -> TzEnv Signature
    signWithLedger ledgerAccount bytes = do
      putStrLn $ "Signing with secret key " <> ledgerAccount <> " stored on Ledger"
      putStrLn $ ("Please approve signing operation on Ledger" :: Text)
      signWithTezosClient ledgerAccount bytes

    signEncrypted :: Text -> ByteString -> TzEnv Signature
    signEncrypted encryptedSk bytes =
      let attempt n = do
            mbPass <- liftIO
              $ runInputT defaultSettings (getPassword (Just '*')
              $ "Please, enter the password for " <> toString alias <> ": ")
            when (isNothing mbPass) $ fail "Failed to enter password"
            eErrSkBytes <- liftIO $ decipherTzEncKey  encryptedSk (fromJust $ fromString <$> mbPass)
            case eErrSkBytes of
              Left err ->
                if n > 0
                then do
                  let nextAttempt = n - 1
                  liftIO . putStrLn . formatPrompt $ nextAttempt
                  attempt nextAttempt
                else fail $ show err
              Right r -> pure $ mySignPK r bytes
      in attempt (5 :: Int)

    formatPrompt n =
      "The password is wrong, try again (" ++ show n ++ " attempt" ++
        if n > 1 then "s remain)" else " remains)"

    signUnencrypted :: Text -> ByteString -> TzEnv Signature
    signUnencrypted plainSk bytes =
      either (fail .  pretty) (pure . flip sign bytes) $
        parseSecretKey plainSk

resolve
  :: ResolveType t -> Text -> TzEnv t
resolve (PkSigAlias bytes) alias = generateSigPK alias bytes
resolve rt alias = do
  let (params, prefix) =
        case rt of
          PublicKeyAlias -> (["show","address"], "Hash: ")
          KeyHashAlias -> (["show","address"], "Public Key: ")
          ContractAlias -> (["show","known","contract"], "")
          AddressAlias -> (["show","address"], "Hash: ")
          _ -> error "resolve: impossible condition"
  out_ <- execSilentWithStdout (params <> [alias])
  key <- T.strip . lineWithPrefix prefix <$> pure out_
  let errLabel = "Failed to resolve alias " <> toString alias
  case rt of
    PublicKeyAlias -> handleErr errLabel $ parsePublicKey key
    KeyHashAlias -> handleErr errLabel $ parseKeyHash key
    AddressAlias -> handleErr errLabel $ parseAddress key
    ContractAlias -> handleErr errLabel $ parseAddress key
    _ -> error "resolve: impossible condition"

handleErr :: Buildable a => String -> Either a t -> TzEnv t
handleErr s = either (fail . ((s <> ": ") <>) . pretty) pure

generateKey
  :: Text -> TzEnv PublicKey
generateKey alias = do
  execSilent [ "gen", "keys", alias ]
  let errLabel = "Error generating key " <> toString alias
  pk <- T.strip . lineWithPrefix "Public Key: " <$> execSilentWithStdout [ "show", "address", alias ]
  handleErr errLabel $ parsePublicKey pk

importSecretKey :: Text -> SecretKey -> TzEnv Address
importSecretKey alias sk = do
  let skUri = "unencrypted:" <> formatSecretKey sk
  output <- execSilentWithStdout $ ["import", "secret", "key", alias, skUri, "--force"]
  let addrStr = words output ^. ix 3
  either (fail . pretty) pure (parseAddress addrStr)

getStorage
  :: forall st.
  ( IsoValue st
  , SingI (ToT st)
  , Typeable (ToT st)
  )
  => Address -> TzEnv st
getStorage addr = do
  output <- execSilentWithStdout $
    ["get", "contract", "storage", "for", formatAddress addr]
  either (fail . pretty) pure $
    parseLorentzValue @st output

stripQuotes :: Text -> Text
stripQuotes = T.dropAround (== '"') . T.strip

getChainId :: Text -> TzEnv ChainId
getChainId name = do
  output <- execSilentWithStdout $
    ["rpc", "get", "/chains/" <> name <> "/chain_id"]

  either (fail . pretty) pure
    (parseChainId . stripQuotes $ output)

getMainChainId :: TzEnv ChainId
getMainChainId = getChainId "main"

getHeadTimestamp :: TzEnv Timestamp
getHeadTimestamp = do
  output <- execSilentWithStdout $ ["get", "timestamp"]
  maybe (fail "Failed to parse timestamp") pure $
    parseTimestamp . T.strip $ output

hashAddressToScriptExpression :: Address -> Text
hashAddressToScriptExpression =
    encodeBase58Check
    -- `script_expr_hash` (`b58check_prefix`) from Tezos sources, see
    -- src/proto_005_PsBabyM1/lib_protocol/script_expr_hash.ml
  . ("\013\044\064\027" <>)
  . blake2b
  . lPackValue

-- NOTE: We don't try to interpret tezos client output,
--   we simply present it to the user.
getElementTextOfBigMapByHash
  :: Text -> Natural -> TzEnv Text
getElementTextOfBigMapByHash thash bigMapId = do
  (o, e) <- exec Silent ["get", "element", thash, "of", "big", "map", show bigMapId]
  return $
    if T.null e
      then T.strip o
      else if "Error:\n  Did not find service:" `T.isPrefixOf` e
              then "0"
              else "Internal error: " <> e

getElementTextOfBigMapByAddress
  :: Address -> Natural -> TzEnv Text
getElementTextOfBigMapByAddress =
  getElementTextOfBigMapByHash . hashAddressToScriptExpression
