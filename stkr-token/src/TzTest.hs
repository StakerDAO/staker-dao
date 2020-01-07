{-# LANGUAGE NoRebindableSyntax #-}

module TzTest
  ( TzTest
  , runTzTest

  , Env(..)
  , readEnvFromFile

  , TransferP(..)
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

import Prelude

import Control.Concurrent (forkIO, killThread)
import qualified Control.Exception as E
import Data.Aeson (FromJSON)
import Data.Singletons (SingI)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Data.Yaml as Yaml
import Fmt (Buildable, pretty)
import Lens.Micro (ix)
import System.Environment (getEnvironment)
import System.Exit (ExitCode)
import System.IO
  (BufferMode(..), hGetBuffering, hGetContents, hPutStr, hSetBuffering)
import System.Process
  (CreateProcess(..), StdStream(..), createProcess, proc, waitForProcess)

import qualified Crypto.Error as CE
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Lorentz
  (Contract, NicePrintedValue, NiceStorage, ParameterEntryPoints,
  parseLorentzValue, lPackValue)
import Lorentz.Print (printLorentzContract, printLorentzValue)
import Michelson.Typed (IsoValue, ToT)
import Tezos.Address (Address, formatAddress, parseAddress)
import Tezos.Core
  (ChainId, Mutez, Timestamp, parseChainId, parseTimestamp, unsafeMkMutez)
import Tezos.Crypto
  (KeyHash, PublicKey(..), SecretKey, Signature(..), blake2b, formatSecretKey,
  parseKeyHash, parsePublicKey, parseSecretKey, sign, toPublic, encodeBase58Check)

import Data.Maybe (fromJust)
import System.Console.Haskeline (defaultSettings, getPassword, runInputT)

import DecipherTzEncKey

data Env = Env
  { envTezosClientCmd :: Text
  , envNode :: Text
  , envNodePort :: Natural
  }
  deriving stock Generic
  deriving anyclass FromJSON

readEnvFromFile :: FilePath -> IO Env
readEnvFromFile path = Yaml.decodeFileThrow @IO @Env path

type TzTest a = ReaderT Env IO a

runTzTest :: TzTest a -> Env -> IO a
runTzTest = runReaderT

-- Flattened
data Verbosity =
    Silent
  | ShowStdOut
  | ShowStdErr
  | ShowBoth
  deriving Eq

exec :: Verbosity -> [Text] -> TzTest (Text, Text)
exec verbosity args =
  execWithShell verbosity args id

execSilentWithStdout :: [Text] -> TzTest Text
execSilentWithStdout args = fst <$> exec Silent args

execSilent :: [Text] -> TzTest ()
execSilent = void . exec Silent

-- For this to work well, you SHALL link with `threaded` RTS!!!
-- Stdin is inherited!
callProcessWithReadStd
  :: Verbosity
  -> FilePath
  -> [String]
  -> Maybe [(String, String)]
  -> IO (ExitCode, String, String)
callProcessWithReadStd verbosity cmd args penv = do
  oBufMode <- hGetBuffering stdout
  eBufMode <- hGetBuffering stderr
  hSetBuffering stdout NoBuffering
  (_, Just hout, Just herr, p) <-
    createProcess $ (proc cmd args) { env = penv
                                    , std_out = CreatePipe
                                    , std_err = CreatePipe
                                    , delegate_ctlc = True}

  let
    geth h ho cref v = do
      ec <- E.try @E.IOException (hGetContents h)
      case ec of
        Right c -> do
          modifyIORef cref (. (++ c))
          if verbosity == ShowBoth || verbosity == v
            then System.IO.hPutStr ho c
            else pure ()
          geth h ho cref v
        _ -> pure ()
  --
  outref <- newIORef id
  errref <- newIORef id
  to <- forkIO $ geth hout stdout outref ShowStdOut
  te <- forkIO $ geth herr stderr errref ShowStdErr
  ec <- waitForProcess p
  killThread to
  killThread te
  hSetBuffering stdout oBufMode
  hSetBuffering stderr eBufMode
  outs <- readIORef outref
  errs <- readIORef errref
  pure (ec, outs [], errs [])

execWithShell
  :: Verbosity
  -> [Text]
  -> (IO (String, String) -> IO (String, String))
  -> TzTest (Text, Text)
execWithShell verbosity args shellTransform = do
  Env{..} <- ask
  let outputShell = do
        let allargs =
              [ "-A", envNode
              , "-P", show envNodePort
              ] <> args
        -- putTextLn $ "EXEC!: " <> envTezosClientCmd <> " " <> T.intercalate " " allargs
        e <- getEnvironment
        -- Ignore exit code ATM
        (_r, o, err) <-
          callProcessWithReadStd
            verbosity
            (T.unpack envTezosClientCmd)
            (map T.unpack allargs)
            (Just $ modenv e)
        -- putStr $ "RECV!: " ++ o
        return (o, err)
  liftIO $ bimap T.pack T.pack <$> shellTransform outputShell
  where
    modenv e = ("TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER", "YES") : e

data TransferP a = TransferP
  { tpQty :: Mutez
  , tpSrc :: Address
  , tpDst :: Address
  , tpBurnCap :: Natural
  , tpArgument :: a
  }

transfer
  :: NicePrintedValue a
  => TransferP a -> TzTest ()
transfer TransferP{..} = do
  execSilent $
    [ "transfer", pretty tpQty
    , "from", formatAddress tpSrc
    , "to", formatAddress tpDst
    , "--burn-cap", show tpBurnCap
    , "--arg", toStrict $ printLorentzValue True tpArgument
    ]

call
  :: NicePrintedValue p
  => Address
  -> Address
  -> p
  -> TzTest ()
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
  , ocpContract :: Contract p st
  , ocpInitalStorage :: st
  , ocpBurnCap :: Natural
  }

originateContract
  :: forall p st.
  ( NiceStorage st
  , ParameterEntryPoints p
  )
  => OriginateContractP p st -> TzTest Address
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
                lineWithPrefix "New contract " . fst <$> exec ShowStdOut cmdArgs
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
  :: ResolveType t -> OrAlias t -> TzTest t
resolve' _ (Value v) = pure v
resolve' rt (Alias al) = resolve rt al

lineWithPrefix :: Text -> Text -> Text
lineWithPrefix prefix txt
  | T.null prefix = txt
  | otherwise =
      fromMaybe "" . safeHead $
      mapMaybe (T.stripPrefix prefix) (lines txt)

-- Tezos.Crypto exports SecretKey abstractly, don't bother to change it
mySignPK :: ByteString -> ByteString -> (PublicKey, Signature)
mySignPK skBytes bytes = (PublicKey pk, Signature . Ed25519.sign sk pk . blake2b $ bytes)
  where
    sk = CE.throwCryptoError $ Ed25519.secretKey skBytes
    pk = Ed25519.toPublic sk

generateSigPK
  :: Text -> ByteString -> TzTest (PublicKey, Signature)
generateSigPK alias bytes = do
  tzsk <- T.strip . lineWithPrefix "Secret Key: " <$> execSilentWithStdout ["show","address", alias, "-S"]
  if "encrypted:" `T.isPrefixOf` tzsk
    then
      let attempt n = do
            mbPass <- liftIO
              $ runInputT defaultSettings (getPassword (Just '*')
              $ "Please, enter the password for " <> toString alias <> ": ")
            when (isNothing mbPass) $ fail "Failed to enter password"
            eErrSkBytes <- liftIO
              $ decipherTzEncKey (T.drop (length @String "encrypted:") tzsk) (fromJust $ fromString <$> mbPass)
            case eErrSkBytes of
              Left err -> if n > 0
                            then
                              let nn = n - 1 in
                               do liftIO $ putStrLn $ formatPrompt nn
                                  attempt nn
                            else
                              fail $ show err
              Right r -> return $ mySignPK r bytes
      in attempt (5 :: Int)
    else
      -- we don't bother to check "unencrypted:" prefix, if it's
      --   different, the parse fails anyway
      let skt = parseSecretKey (T.drop (length @String "unencrypted:") tzsk)
      in case skt of
           Left err -> fail $ show err
           Right sk -> return (toPublic sk, sign sk bytes)
   where
     formatPrompt n =
       "The password is wrong, try again (" ++ show n ++ " attempt" ++
          if n > 1 then "s remain)" else " remains)"


resolve
  :: ResolveType t -> Text -> TzTest t
resolve (PkSigAlias bytes) alias = generateSigPK alias bytes
resolve rt alias = do
  let (params, prefix) =
        case rt of
          PublicKeyAlias -> (["show","address"], "Hash: ")
          KeyHashAlias -> (["show","address"], "Public Key: ")
          ContractAlias -> (["show","known","contract"], "")
          AddressAlias -> (["show","address"], "Hash: ")
          _ -> error "resolve: impossible condition"
  key <- T.strip . lineWithPrefix prefix <$> execSilentWithStdout (params <> [alias])
  let errLabel = "Failed to resolve alias " <> toString alias
  case rt of
    PublicKeyAlias -> handleErr errLabel $ parsePublicKey key
    KeyHashAlias -> handleErr errLabel $ parseKeyHash key
    AddressAlias -> handleErr errLabel $ parseAddress key
    ContractAlias -> handleErr errLabel $ parseAddress key
    _ -> error "resolve: impossible condition"

handleErr :: Buildable a => String -> Either a t -> TzTest t
handleErr s = either (fail . ((s <> ": ") <>) . pretty) pure

generateKey
  :: Text -> TzTest PublicKey
generateKey alias = do
  execSilent [ "gen", "keys", alias ]
  let errLabel = "Error generating key " <> toString alias
  pk <- T.strip . lineWithPrefix "Public Key: " <$> execSilentWithStdout [ "show", "address", alias ]
  handleErr errLabel $ parsePublicKey pk

importSecretKey :: Text -> SecretKey -> TzTest Address
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
  => Address -> TzTest st
getStorage addr = do
  output <- execSilentWithStdout $
    ["get", "contract", "storage", "for", formatAddress addr]
  either (fail . pretty) pure $
    parseLorentzValue @st output

stripQuotes :: Text -> Text
stripQuotes = T.dropAround (== '"') . T.strip

getChainId :: Text -> TzTest ChainId
getChainId name = do
  output <- execSilentWithStdout $
    ["rpc", "get", "/chains/" <> name <> "/chain_id"]

  either (fail . pretty) pure
    (parseChainId . stripQuotes $ output)

getMainChainId :: TzTest ChainId
getMainChainId = getChainId "main"

getHeadTimestamp :: TzTest Timestamp
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
  :: Text -> Natural -> TzTest Text
getElementTextOfBigMapByHash thash bigMapId = do
  (o, e) <- exec Silent ["get", "element", thash, "of", "big", "map", show bigMapId]
  return $
    if T.null e
      then T.strip o
      else if "Error:\n  Did not find service:" `T.isPrefixOf` e
              then "0"
              else "Internal error: " <> e

getElementTextOfBigMapByAddress
  :: Address -> Natural -> TzTest Text
getElementTextOfBigMapByAddress =
   getElementTextOfBigMapByHash . hashAddressToScriptExpression
