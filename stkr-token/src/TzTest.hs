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
  , getElementOfBigMapByAddress

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
import Text.Hex (encodeHex)
import Lens.Micro (ix)

import Lorentz (Contract, NicePrintedValue, NiceStorage, ParameterEntryPoints, parseLorentzValue)
import Lorentz.Print (printLorentzContract, printLorentzValue)
import Michelson.Typed (IsoValue, ToT)
import Tezos.Address (Address, formatAddress, parseAddress)
import Tezos.Core (Mutez, ChainId, parseChainId, unsafeMkMutez, parseTimestamp, Timestamp)
import Tezos.Crypto (PublicKey, KeyHash, Signature, parsePublicKey, parseKeyHash, parseSignature, SecretKey, formatSecretKey)

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

resolve
  :: ResolveType t -> Text -> TzTest t
resolve rt alias = do
  let (params, prefix) =
        case rt of
          PublicKeyAlias -> (["show","address"], "Hash: ")
          KeyHashAlias -> (["show","address"], "Public Key: ")
          ContractAlias -> (["show","known","contract"], "")
          AddressAlias -> (["show","address"], "Hash: ")
          PkSigAlias _ -> (["show","address"], "Public Key: ")
  key <- T.strip . lineWithPrefix prefix <$> exec (params <> [alias])
  -- putTextLn $ "line: " <> key
  let errLabel = "Failed to resolve alias " <> toString alias
  case rt of
    PublicKeyAlias -> handleErr errLabel $ parsePublicKey key
    KeyHashAlias -> handleErr errLabel $ parseKeyHash key
    PkSigAlias bytes ->
      (,) <$> handleErr errLabel (parsePublicKey key)
          <*> generateSig alias bytes
    AddressAlias -> handleErr errLabel $ parseAddress key
    ContractAlias -> handleErr errLabel $ parseAddress key

handleErr :: Buildable a => String -> Either a t -> TzTest t
handleErr s = either (fail . ((s <> ": ") <>) . pretty) pure

generateSig
  :: Text -> ByteString -> TzTest Signature
generateSig alias bytes = do
  let errLabel = ("Failed to sign "<>toString bytes_
                    <>" with alias "<> toString alias)
  sgn <- T.strip . lineWithPrefix "Signature: " <$>
          exec ["sign", "bytes", "0x"<>bytes_, "for", alias]
  handleErr errLabel (parseSignature sgn)
  where
    bytes_ = encodeHex bytes

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

-- FIXME? Move to Morley?
type Parsable t =
  ( IsoValue t
  , SingI (ToT t)
  , Typeable (ToT t)
  )

getStorage
  :: forall st. Parsable st
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

hashAddressToScriptExpression :: Address -> TzTest Text
hashAddressToScriptExpression addr =
  T.strip . lineWithPrefix "Script-expression-ID-Hash: " <$>
     exec ["hash", "data", "\"" <> formatAddress addr <> "\"", "of", "type", "address"]

getElementTextOfBigMapByHash
  :: Text -> Natural -> TzTest Text
getElementTextOfBigMapByHash thash bigMapId = do
  -- FIXME??? I haven't tested it (it fails) and don't yet know what the output
  --   should be prefixed with (if any), so I assume empty prefix ATM.
  T.strip . lineWithPrefix "" <$>
     exec ["get", "element", thash, "of", "big", "map", show bigMapId]

getElementOfBigMapByHash
  :: forall e. Parsable e
  => Text -> Natural -> TzTest e
getElementOfBigMapByHash thash bigMapId = do
  eltext <- getElementTextOfBigMapByHash thash bigMapId
  either (fail . pretty) pure $
    parseLorentzValue @e eltext

getElementTextOfBigMapByAddress
  :: Address -> Natural -> TzTest Text
getElementTextOfBigMapByAddress addr bigMapId =
  hashAddressToScriptExpression addr >>= (`getElementTextOfBigMapByHash` bigMapId)

getElementOfBigMapByAddress
  :: forall e. Parsable e
  => Address -> Natural -> TzTest e
getElementOfBigMapByAddress addr bigMapId =
  hashAddressToScriptExpression addr >>= (`getElementOfBigMapByHash` bigMapId)
