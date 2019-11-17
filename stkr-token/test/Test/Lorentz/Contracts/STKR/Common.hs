module Test.Lorentz.Contracts.STKR.Common
  ( originate
  , newKeypair
  , multisignValue
  ) where

import Lorentz.Constraints
import Lorentz.Pack (lPackValue)
import Lorentz.Test
import Lorentz.Value
import Tezos.Crypto (PublicKey, SecretKey, Signature, detSecretKey, sign, toPublic)

import qualified Lorentz.Contracts.STKR as STKR

originate
  :: STKR.Storage
  -> IntegrationalScenarioM (ContractAddr STKR.Parameter)
originate storage = lOriginate STKR.stkrContract "STKR token" storage (toMutez 1000)

newKeypair :: ByteString -> (SecretKey, PublicKey)
newKeypair bs = let sk = detSecretKey bs in (sk, toPublic sk)

multisignValue
  :: NicePackedValue a
  => [SecretKey] -- Sks to be signed with
  -> a           -- Value to be signed
  -> [(PublicKey, Signature)]
multisignValue opsSks newCouncil =
  let packedCouncil = lPackValue newCouncil
  in (\sk -> (toPublic sk, sign sk packedCouncil)) <$> opsSks
