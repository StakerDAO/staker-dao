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

  , resolve
  , resolve'
  , ResolveType (..)
  , OrAlias (..)
  ) where

import Prelude

import Data.Singletons (SingI)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import Fmt (Buildable, pretty)
import Turtle (Line, Shell)
import qualified Turtle
import Data.Aeson (FromJSON)
import qualified Data.Yaml as Yaml
import Lens.Micro (ix)

import Lorentz (Contract, NicePrintedValue, NiceStorage, ParameterEntryPoints, parseLorentzValue)
import Lorentz.Print (printLorentzContract, printLorentzValue)
import Michelson.Typed (IsoValue, ToT)
import Tezos.Address (Address, formatAddress, parseAddress)
import Tezos.Core (Mutez, ChainId, parseChainId, unsafeMkMutez, parseTimestamp, Timestamp)
import Tezos.Crypto (PublicKey(..), KeyHash, Signature(..), parsePublicKey, parseKeyHash, SecretKey,
  parseSecretKey, toPublic, sign, formatSecretKey, blake2b)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.Error as CE

import Data.Maybe (fromJust)
import System.Console.Haskeline (runInputT, defaultSettings, getPassword)

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

exec :: [Text] -> TzTest Text
exec args = execWithShell args id

execWithShell :: [Text] -> (Shell Line -> Shell Line) -> TzTest Text
execWithShell args shellTransform = do
  Env{..} <- ask
  Turtle.export "TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER" "YES"
  -- putTextLn $ "Executing command: " <> T.intercalate " " args
  let outputShell =
        Turtle.inproc envTezosClientCmd
          ([ "-A", envNode
           , "-P", show envNodePort
           ] <> args)
          Turtle.empty
  liftIO . Turtle.strict . shellTransform $ outputShell

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
  exec $
    [ "transfer", pretty tpQty
    , "from", formatAddress tpSrc
    , "to", formatAddress tpDst
    , "--burn-cap", show tpBurnCap
    , "--arg", toStrict $ printLorentzValue True tpArgument
    ]
  pure ()

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
                lineWithPrefix "New contract " <$> exec cmdArgs
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
  tzsk <- T.strip . lineWithPrefix "Secret Key: " <$> exec ["show","address", alias, "-S"]
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
  key <- T.strip . lineWithPrefix prefix <$> exec (params <> [alias])
  -- putTextLn $ "line: " <> key
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
  _ <- exec [ "gen", "keys", alias ]
  let errLabel = "Error generating key " <> toString alias
  pk <- T.strip . lineWithPrefix "Public Key: " <$> exec [ "show", "address", alias ]
  handleErr errLabel $ parsePublicKey pk

importSecretKey :: Text -> SecretKey -> TzTest Address
importSecretKey alias sk = do
  let skUri = "unencrypted:" <> formatSecretKey sk
  output <- exec $ ["import", "secret", "key", alias, skUri, "--force"]
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
  output <- exec $
    ["get", "contract", "storage", "for", formatAddress addr]
  either (fail . pretty) pure $
    parseLorentzValue @st output
stripQuotes :: Text -> Text
stripQuotes = T.dropAround (== '"') . T.strip

getChainId :: Text -> TzTest ChainId
getChainId name = do
  output <- exec $
    ["rpc", "get", "/chains/" <> name <> "/chain_id"]

  either (fail . pretty) pure
    (parseChainId . stripQuotes $ output)

getMainChainId :: TzTest ChainId
getMainChainId = getChainId "main"

getHeadTimestamp :: TzTest Timestamp
getHeadTimestamp = do
  output <- exec $ ["get", "timestamp"]
  maybe (fail "Failed to parse timestamp") pure $
    parseTimestamp . T.strip $ output
