module Test.Lorentz.Contracts.STKR.NewCouncil
  ( spec_NewCouncil
  ) where

import Prelude

import qualified Data.Set as S
import Fmt (listF, (+|), (|+))
import Lorentz (fromContractAddr)
import Lorentz.Test
import Test.Hspec (Spec, it)
import Tezos.Crypto (hashKey)

import qualified Lorentz.Contracts.STKR as STKR

import Test.Lorentz.Contracts.STKR.Common (callWithMultisig, newKeypair, originateWithEmptyLedger)


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
      let newCouncilKeys = S.fromList [hashKey pk6, hashKey pk7]
      (msig, stkr) <- originateWithEmptyLedger teamPks []

      callWithMultisig msig 1 teamSks stkr $ STKR.NewCouncil newCouncilKeys

      validate . Right . lExpectStorageUpdate stkr $ \storage ->
        if newCouncilKeys == (STKR.councilKeys storage)
        then pass
        else Left . CustomValidationError $
               "Expected " +| listF newCouncilKeys |+
               ", but got " +| listF (STKR.councilKeys storage) |+ ""

  it "fails if called directly" $
    integrationalTestExpectation $ do
      let teamPks = [pk1, pk2, pk3, pk4, pk5]
      let newCouncilKeys = S.fromList [hashKey pk6, hashKey pk7]
      (msig, stkr) <- originateWithEmptyLedger teamPks []

      lCall stkr $
        STKR.OpsTeamEntrypoint .
        STKR.EnsureOwner .
        STKR.NewCouncil $ newCouncilKeys
      validate . Left $
        lExpectCustomError #senderCheckFailed (fromContractAddr msig)
