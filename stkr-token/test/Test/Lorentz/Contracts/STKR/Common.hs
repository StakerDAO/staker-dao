module Test.Lorentz.Contracts.STKR.Common
  ( originate
  , newKeypair
  ) where

import qualified Data.Set as Set

import Lorentz.Test
import Lorentz.Value
import Tezos.Crypto (PublicKey, SecretKey, detSecretKey, hashKey, toPublic)

import qualified Lorentz.Contracts.Multisig as Multisig
import qualified Lorentz.Contracts.STKR as STKR

originate
  :: [PublicKey] -> [PublicKey]
  -> IntegrationalScenarioM (ContractRef Multisig.Parameter, ContractRef STKR.Parameter)
originate teamKeys councilKeys = do
  msig <- lOriginate Multisig.multisigContract "Operation team multisig"
            Multisig.Storage
              { teamKeys = Set.fromList $ hashKey <$> teamKeys
              , currentNonce = 0
              }
            (toMutez 0)
  stkr <- lOriginate STKR.stkrContract "STKR token"
            STKR.Storage
              { owner = fromContractAddr msig
              , councilKeys = councilKeys
              , urls = mempty
              }
            (toMutez 0)
  return (msig, stkr)

newKeypair :: ByteString -> (SecretKey, PublicKey)
newKeypair bs = let sk = detSecretKey bs in (sk, toPublic sk)
