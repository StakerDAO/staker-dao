module Test.Lorentz.Contracts.STKR.Common
  ( originate
  , newKeypair
  ) where

import qualified Data.Set as Set

import Lorentz.Constraints
import Lorentz.Pack (lPackValue)
import Lorentz.Test
import Lorentz.Value
import Tezos.Crypto (PublicKey, SecretKey, Signature, detSecretKey, hashKey, sign, toPublic)

import qualified Lorentz.Contracts.Multisig as Multisig
import qualified Lorentz.Contracts.STKR as STKR

originate
  :: Address -> [PublicKey] -> [PublicKey]
  -> IntegrationalScenarioM (ContractRef Multisig.Parameter, ContractRef STKR.Parameter)
originate admin teamKeys councilKeys = do
  stkr <- lOriginate STKR.stkrContract "STKR token"
            STKR.Storage
              { owner = admin
              , team = Nothing
              , councilKeys = councilKeys
              , urls = mempty
              }
            (toMutez 0)
  msig <- lOriginate Multisig.multisigContract "Operation team multisig"
            Multisig.Storage
              { keys = Set.fromList $ hashKey <$> teamKeys
              , quorum = fromIntegral $ (length teamKeys) `ceilDiv` 2
              , currentNonce = 0
              , stakerAddress = fromContractAddr stkr
              }
            (toMutez 0)
  lCall stkr $
    STKR.SetOperationsTeam (fromContractAddr msig)
  return (msig, stkr)
  where
    ceilDiv a b = -((-a) `div` b)  -- rounds toward +inf

newKeypair :: ByteString -> (SecretKey, PublicKey)
newKeypair bs = let sk = detSecretKey bs in (sk, toPublic sk)
