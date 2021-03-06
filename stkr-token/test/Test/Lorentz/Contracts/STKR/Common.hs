{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Test.Lorentz.Contracts.STKR.Common
  ( OriginateParams (..)
  , originate
  , originateWithTC
  , originateWithEmptyLedger

  , callWithMultisig
  , callWithMultisig'

  , failWhenNot
  , expectSuccess

  , newKeypair
  , mkTeamKeys
  , wallet1
  , wallet2
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import CryptoInterop (PublicKey(..), SecretKey, detSecretKey, hashKey, toPublic)
import qualified Data.ByteString as BS
import Lens.Micro.Internal (At(..), Index, IxValue, Ixed(..))
import Lorentz.Contracts.Client (multisignValue)
import Lorentz.Contracts.Multisig
  (OrderDest(..), TransferOrderWrapC, mkCallOrderWrap)
import Lorentz.Test
import Lorentz.Value
import Michelson.Test.Dummy (dummyNow)
import Named (Name(..), NamedF)
import Test.QuickCheck (Arbitrary(..), arbitrarySizedNatural)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary, genericArbitrary)
import Util.Named ((.!))

import qualified Lorentz.Contracts.Multisig as Multisig
import qualified Lorentz.Contracts.STKR as STKR

defTimeConfig :: STKR.TimeConfig
defTimeConfig = STKR.TestTC { _start = dummyNow, _stageDuration = 1 }

originateWithTC
  :: STKR.TimeConfig
  -> OriginateParams
  -> IntegrationalScenarioM (TAddress Multisig.Parameter, TAddress STKR.Parameter)
originateWithTC tc OriginateParams{..} = do
  msig <- lOriginate Multisig.multisigContract "Operation team multisig"
            Multisig.Storage
              { teamKeys = Set.fromList $ hashKey <$> opTeamKeys
              , currentNonce = 0
              }
            (toMutez 0)
  stkr <- lOriginate (STKR.stkrContract tc) "STKR token"
            STKR.Storage
              { owner = unTAddress msig
              , councilKeys = Set.fromList (hashKey <$> opCouncilKeys)
              , proposals = []
              , votes = Map.empty
              , policy = #urls Map.empty
              , stageCounter = 0
              , totalSupply = getSum $ foldMap (Sum . snd) opInitailLedger
              , ledger = BigMap . Map.fromList $ opInitailLedger
              , frozen = False
              , successor = Nothing
              }
            (toMutez 0)
  return (msig, stkr)

data OriginateParams = OriginateParams
  { opTeamKeys :: [PublicKey]
  , opCouncilKeys :: [PublicKey]
  , opInitailLedger :: [(Address, Natural)]
  }

originate
  :: OriginateParams
  -> IntegrationalScenarioM (TAddress Multisig.Parameter, TAddress STKR.Parameter)
originate params = do
  setNow dummyNow
  originateWithTC defTimeConfig params

originateWithEmptyLedger
  :: [PublicKey] -> [PublicKey]
  -> IntegrationalScenarioM (TAddress Multisig.Parameter, TAddress STKR.Parameter)
originateWithEmptyLedger teamKeys councilKeys = originate $
  OriginateParams
    { opTeamKeys = teamKeys
    , opCouncilKeys = councilKeys
    , opInitailLedger = mempty
    }

-- | An utility function that creates a call order, signs it and
-- calls Multisig with the correct parameter. An extended version
-- that also accepts the wrapper label (i.e. #cOpsTeamEntrypoint,
-- #cPermitOnFrozen).
callWithMultisig'
  :: forall wrapper param. TransferOrderWrapC STKR.Parameter wrapper (STKR.EnsureOwner param)
  => TAddress Multisig.Parameter
  -> Label wrapper
  -> Natural
  -> [SecretKey]
  -> TAddress STKR.Parameter
  -> param
  -> IntegrationalScenarioM ()
callWithMultisig' msig paramWrapper nonce teamSecretKeys stkr param = do
  let order = mkCallOrderWrap @STKR.Parameter (Ref stkr) paramWrapper $ STKR.EnsureOwner param
  let toSign = Multisig.ValueToSign
        { vtsMultisigAddress = unTAddress msig
        , vtsNonce = nonce
        , vtsOrder = order
        }

  lCallDef msig $
    Multisig.Parameter
      { order = order
      , nonce = nonce
      , signatures = multisignValue teamSecretKeys toSign
      }

-- | An utility function that creates a call order, signs it and
-- calls Multisig with the correct parameter.
callWithMultisig
  :: TAddress Multisig.Parameter
  -> Natural
  -> [SecretKey]
  -> TAddress STKR.Parameter
  -> STKR.OpsTeamEntrypointParam
  -> IntegrationalScenarioM ()
callWithMultisig msig = callWithMultisig' msig #cOpsTeamEntrypoint

newKeypair :: ByteString -> (SecretKey, PublicKey)
newKeypair bs = let sk = detSecretKey bs in (sk, toPublic sk)

mkTeamKeys :: [(SecretKey, PublicKey)]
mkTeamKeys = newKeypair <$> ["1", "2", "3", "4", "5"]

wallet1, wallet2 :: Address
wallet1 = genesisAddress1
wallet2 = genesisAddress2

type instance Index (BigMap k v) = k
type instance IxValue (BigMap k v) = v

instance Ord k => Ixed (BigMap k v) where
  ix k f m@(BigMap im) = case Map.lookup k im of
     Just v  -> f v <&> \v' -> BigMap $ Map.insert k v' im
     Nothing -> pure m

instance Ord k => At (BigMap k v) where
  at k f m@(BigMap im) = f mv <&> \r -> case r of
    Nothing -> maybe m (const (BigMap $ Map.delete k im)) mv
    Just v' -> BigMap $ Map.insert k v' im
    where mv = Map.lookup k im

expectSuccess :: Either ValidationError () -> SuccessValidator
expectSuccess body _ _ _ = body

failWhenNot :: Bool -> Text -> Either ValidationError ()
failWhenNot cond message = when (not cond) (Left . CustomValidationError $ message)

instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural

instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary

instance Arbitrary a => Arbitrary (NamedF Identity a name) where
  arbitrary = (Name @name .!) <$> arbitrary @a

instance Arbitrary STKR.OpsTeamEntrypointParam where
  arbitrary = genericArbitrary

instance ToADTArbitrary STKR.OpsTeamEntrypointParam

deriving newtype instance Arbitrary STKR.Sha256Hash
deriving stock instance Show STKR.OpsTeamEntrypointParam
