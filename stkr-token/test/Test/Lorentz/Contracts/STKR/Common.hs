{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Test.Lorentz.Contracts.STKR.Common
  ( OriginateParams (..)
  , originate
  , originateWithEmptyLedger

  , callWithMultisig
  , callSetSuccessor

  , failWhenNot
  , expectSuccess

  , newKeypair
  , wallet1
  , wallet2
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Lens.Micro.Internal (At(..), Index, IxValue, Ixed(..))
import Lorentz (Lambda)
import Lorentz.Contracts.Client (multisignValue)
import Lorentz.Contracts.Multisig (OrderDest(..), mkCallOrderWrap)
import Lorentz.Test
import Lorentz.Value
import Michelson.Test.Dummy (dummyNow)
import Tezos.Crypto (PublicKey, SecretKey, detSecretKey, hashKey, toPublic)
import Util.Named ((.!))

import qualified Lorentz.Contracts.Multisig as Multisig
import qualified Lorentz.Contracts.STKR as STKR

defTimeConfig :: STKR.TimeConfig
defTimeConfig = STKR.TestTC { _start = dummyNow, _stageDuration = 1 }

data OriginateParams = OriginateParams
  { opTeamKeys :: [PublicKey]
  , opCouncilKeys :: [PublicKey]
  , opInitailLedger :: [(Address, Natural)]
  }

originate
  :: OriginateParams
  -> IntegrationalScenarioM (ContractRef Multisig.Parameter, ContractRef STKR.Parameter)
originate OriginateParams{..} = do
  setNow dummyNow
  msig <- lOriginate Multisig.multisigContract "Operation team multisig"
            Multisig.Storage
              { teamKeys = Set.fromList $ hashKey <$> opTeamKeys
              , currentNonce = 0
              }
            (toMutez 0)
  stkr <- lOriginate (STKR.stkrContract defTimeConfig) "STKR token"
            STKR.Storage
              { owner = fromContractAddr msig
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

originateWithEmptyLedger
  :: [PublicKey] -> [PublicKey]
  -> IntegrationalScenarioM (ContractRef Multisig.Parameter, ContractRef STKR.Parameter)
originateWithEmptyLedger teamKeys councilKeys = originate $
  OriginateParams
    { opTeamKeys = teamKeys
    , opCouncilKeys = councilKeys
    , opInitailLedger = mempty
    }

-- | An utility function that creates a call order, signs it and
-- calls Multisig with the correct parameter.
callWithMultisig
  :: ContractRef Multisig.Parameter
  -> Natural
  -> [SecretKey]
  -> ContractRef STKR.Parameter
  -> STKR.OpsTeamEntrypointParam
  -> IntegrationalScenarioM ()
callWithMultisig msig nonce teamSecretKeys stkr param = do
  let order = mkCallOrderWrap (Ref stkr) #cOpsTeamEntrypoint $ STKR.EnsureOwner param
  let toSign = Multisig.ValueToSign
        { vtsMultisigAddress = fromContractAddr msig
        , vtsNonce = nonce
        , vtsOrder = order
        }

  lCall msig $
    Multisig.Parameter
      { order = order
      , nonce = nonce
      , signatures = multisignValue teamSecretKeys toSign
      }

callSetSuccessor
  :: ContractRef Multisig.Parameter
  -> Natural
  -> [SecretKey]
  -> ContractRef STKR.Parameter
  -> Lambda STKR.PublicEntrypointParam Operation
  -> IntegrationalScenarioM ()
callSetSuccessor msig nonce teamSecretKeys stkr lambda = do
  let order = mkCallOrderWrap (Ref stkr) #cSetSuccessor $ STKR.EnsureOwner (#successor .! lambda)
  let toSign = Multisig.ValueToSign
        { vtsMultisigAddress = fromContractAddr msig
        , vtsNonce = nonce
        , vtsOrder = order
        }

  lCall msig $
    Multisig.Parameter
      { order = order
      , nonce = nonce
      , signatures = multisignValue teamSecretKeys toSign
      }

newKeypair :: ByteString -> (SecretKey, PublicKey)
newKeypair bs = let sk = detSecretKey bs in (sk, toPublic sk)

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
