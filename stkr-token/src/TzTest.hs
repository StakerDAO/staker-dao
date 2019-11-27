module TzTest
  ( TzTest
  , Env(..)
  , runTzTest

  , TransferP(..)
  , transfer

  , OriginateContractP(..)
  , originateContract

  , generateKey
  , getStorage
  ) where

import Prelude

import Data.Singletons (SingI)
import Data.Text.Lazy (toStrict)
import Fmt (pretty)
import Lens.Micro (ix, (^.))
import Turtle (Line, Shell)
import qualified Turtle
import Data.Aeson (FromJSON)

import Lorentz (Contract, NicePrintedValue, NiceStorage, ParameterEntryPoints, parseLorentzValue)
import Lorentz.Print (printLorentzContract, printLorentzValue)
import Michelson.Typed (IsoValue, ToT)
import Tezos.Address (Address, formatAddress, parseAddress)
import Tezos.Core (Mutez)
import Tezos.Crypto (PublicKey, parsePublicKey)

data Env = Env
  { envTezosClientCmd :: Text
  , envNode :: Text
  , envNodePort :: Natural
  }
  deriving stock Generic
  deriving anyclass FromJSON

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
  { ocpName :: Text
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
        [ "originate", "contract", ocpName
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
