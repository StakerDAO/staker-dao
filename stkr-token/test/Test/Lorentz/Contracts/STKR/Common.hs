module Test.Lorentz.Contracts.STKR.Common
  ( originate
  , newKeypair
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Lorentz.Test
import Lorentz.Value
import Tezos.Crypto (PublicKey, SecretKey, detSecretKey, hashKey, toPublic)
import Michelson.Test.Dummy (dummyNow)

import qualified Lorentz.Contracts.Multisig as Multisig
import qualified Lorentz.Contracts.STKR as STKR

defTimeConfig :: STKR.TimeConfig
defTimeConfig = STKR.TestTC { _start = dummyNow, _stageDuration = 1 }

originate
  :: [PublicKey] -> [PublicKey]
  -> IntegrationalScenarioM (ContractRef Multisig.Parameter, ContractRef STKR.Parameter)
originate teamKeys councilKeys = do
  setNow dummyNow
  msig <- lOriginate Multisig.multisigContract "Operation team multisig"
            Multisig.Storage
              { teamKeys = Set.fromList $ hashKey <$> teamKeys
              , currentNonce = 0
              }
            (toMutez 0)
  stkr <- lOriginate (STKR.stkrContract defTimeConfig) "STKR token"
            STKR.Storage
              { owner = fromContractAddr msig
              , councilKeys = Set.fromList (hashKey <$> councilKeys)
              , proposals = []
              , votes = Map.empty
              , policy = #urls Map.empty
              , stageCounter = 0
              }
            (toMutez 0)
  return (msig, stkr)

newKeypair :: ByteString -> (SecretKey, PublicKey)
newKeypair bs = let sk = detSecretKey bs in (sk, toPublic sk)
