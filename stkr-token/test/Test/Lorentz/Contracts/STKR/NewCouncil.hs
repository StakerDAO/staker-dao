module Test.Lorentz.Contracts.STKR.NewCouncil
  ( spec_NewCouncil
  ) where

import Lorentz (fromContractAddr)
import Prelude

import Fmt (listF, (+|), (|+))
import qualified Lorentz as L
import Lorentz.Test
import Test.Hspec (Spec, it)
import Tezos.Core (dummyChainId)
import Tezos.Crypto (SecretKey)

import Lorentz.Contracts.Multisig (mkCallOrder)
import qualified Lorentz.Contracts.Multisig as Multisig
import qualified Lorentz.Contracts.STKR as STKR
import Lorentz.Contracts.Client (multisignValue)

import Test.Lorentz.Contracts.STKR.Common (newKeypair, originate)

-- | An utility function that creates a call order, signs it and
-- calls Multisig with the correct parameter.
callWithMultisig
  :: L.ContractRef Multisig.Parameter
  -> Natural
  -> [SecretKey]
  -> L.ContractRef STKR.Parameter
  -> STKR.Parameter
  -> IntegrationalScenarioM ()
callWithMultisig msig nonce teamSecretKeys stkr param = do
  let order = mkCallOrder stkr param
  let toSign = Multisig.ValueToSign
        { vtsChainId = dummyChainId
        , vtsNonce = nonce
        , vtsOrder = order
        }

  lCall msig $
    Multisig.Parameter
      { order = order
      , nonce = nonce
      , signatures = multisignValue teamSecretKeys toSign
      }

spec_NewCouncil :: Spec
spec_NewCouncil = newCouncilSpec

newCouncilSpec :: Spec
newCouncilSpec = do
  let (sk1, pk1) = newKeypair "1"
  let (sk2, pk2) = newKeypair "2"
  let (sk3, pk3) = newKeypair "3"
  let (sk4, pk4) = newKeypair "4"
  let (sk5, pk5) = newKeypair "5"
  let (_, pk6) = newKeypair "6"
  let (_, pk7) = newKeypair "7"
  it "updates council keys if called via multisig" $
    integrationalTestExpectation $ do
      let teamPks = [pk1, pk2, pk3, pk4, pk5]
      let teamSks = [sk1, sk2, sk3, sk4, sk5]
      let newCouncilKeys = [pk6, pk7]
      (msig, stkr) <- originate teamPks []

      callWithMultisig msig 1 teamSks stkr $
        STKR.NewCouncil newCouncilKeys

      validate . Right . lExpectStorageUpdate stkr $ \storage ->
        if newCouncilKeys == (STKR.councilKeys storage)
        then pass
        else Left . CustomValidationError $
               "Expected " +| listF newCouncilKeys |+
               ", but got " +| listF (STKR.councilKeys storage) |+ ""

  it "fails if called directly" $
    integrationalTestExpectation $ do
      let teamPks = [pk1, pk2, pk3, pk4, pk5]
      let newCouncilKeys = [pk6, pk7]
      (msig, stkr) <- originate teamPks []

      lCall stkr $
        STKR.NewCouncil newCouncilKeys

      validate . Left $
        lExpectCustomError #senderCheckFailed (fromContractAddr msig)
