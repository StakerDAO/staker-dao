module TzTest
  ( TzTest
  , runTzTest

  , Env(..)
  , readEnvFromFile

  , TransferP(..)
  , transfer

  , OriginateContractP(..)
  , originateContract

  , generateKey
  , importSecretKey
  , getStorage

  , getChainId
  , getMainChainId
  ) where

import Prelude

import Data.Singletons (SingI)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import Fmt (pretty)
import Lens.Micro (ix, (^.))
import Turtle (Line, Shell)
import qualified Turtle
import Data.Aeson (FromJSON)
import qualified Data.Yaml as Yaml

import Lorentz (Contract, NicePrintedValue, NiceStorage, ParameterEntryPoints, parseLorentzValue)
import Lorentz.Print (printLorentzContract, printLorentzValue)
import Michelson.Typed (IsoValue, ToT)
import Tezos.Address (Address, formatAddress, parseAddress)
import Tezos.Core (Mutez, ChainId, parseChainId)
import Tezos.Crypto (PublicKey, parsePublicKey, SecretKey, formatSecretKey)

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
  contractLine <- execWithShell cmdArgs $
    Turtle.grep (Turtle.prefix "New contract")
  -- Ex: New contract KT1MNzB6r9eFiYtFbhnRUgnuC83vwSUqERWG originated.
  let addrString = (words contractLine) ^. ix 2
  either (fail . pretty) pure $ parseAddress addrString

generateKey
  :: Text -> TzTest PublicKey
generateKey alias = do
  _ <- exec [ "gen", "keys", alias ]
  keyLine <- execWithShell [ "show", "address", alias ] $
    Turtle.grep (Turtle.prefix "Public Key:")
  -- Ex: New contract KT1MNzB6r9eFiYtFbhnRUgnuC83vwSUqERWG originated.
  let pk = (words keyLine) ^. ix 2
  either (fail . (("Error for PK " <> toString pk) <>) . pretty) pure $ parsePublicKey pk

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
