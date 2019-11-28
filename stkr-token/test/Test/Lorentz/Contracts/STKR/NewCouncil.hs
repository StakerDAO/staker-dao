module Test.Lorentz.Contracts.STKR.NewCouncil
  ( spec_NewCouncil
  ) where

import Lorentz (Address)
import Prelude

import Fmt (listF, (+|), (|+))
import Lens.Micro
import Lorentz.Pack
import Lorentz.Test
import Test.Hspec (Spec, it)
import Tezos.Core (dummyChainId)
import Tezos.Crypto (sign)

import qualified Lorentz.Contracts.Multisig as Multisig
import qualified Lorentz.Contracts.STKR as STKR
import Lorentz.Contracts.STKR.Client (multisignValue)

import Test.Lorentz.Contracts.STKR.Common (newKeypair, originate)

admin :: Address
admin = genesisAddress

spec_NewCouncil :: Spec
spec_NewCouncil = newCouncilSpec

newCouncilSpec :: Spec
newCouncilSpec = do
  let (sk1, pk1) = newKeypair "1"
  let (sk2, pk2) = newKeypair "2"
  let (sk3, pk3) = newKeypair "3"
  let (sk4, pk4) = newKeypair "4"
  let (sk5, pk5) = newKeypair "5"
  let (sk6, pk6) = newKeypair "6"
  let (_, pk7) = newKeypair "7"
  it "update council keys if quorum is reached" $
    integrationalTestExpectation $ do
      let teamPks = [pk1, pk2, pk3, pk4, pk5]
      let teamSks = [sk1, sk2, sk3, sk4, sk5]
      let newCouncilKeys = [pk6, pk7]
      let toSign = ((dummyChainId, 1 :: Natural), STKR.NewCouncil newCouncilKeys)
      let correctlySignedKeys = multisignValue teamSks toSign
      (msig, stkr) <- originate admin teamPks []
      lCall msig $
        Multisig.Parameter
          { stakerParam = STKR.NewCouncil newCouncilKeys
          , nonce = 1
          , signatures = correctlySignedKeys
          }

      validate . Right . lExpectStorageUpdate stkr $ \storage ->
        if newCouncilKeys == (STKR.councilKeys storage)
        then pass
        else Left . CustomValidationError $
               "Expected " +| listF newCouncilKeys |+
               ", but got " +| listF (STKR.councilKeys storage) |+ ""

  it "fail if one of signature in approvals is incorrect" $
    integrationalTestExpectation $ do
      let teamPks = [pk1, pk2, pk3, pk4, pk5]
      let teamSks = [sk1, sk2, sk3, sk4, sk5]
      let newCouncilKeys = [pk6, pk7]
      let toSign = ((dummyChainId, 1 :: Natural), STKR.NewCouncil newCouncilKeys)
      let correctlySignedKeys = multisignValue teamSks toSign
      let wrongSignature = sign sk6 (lPackValue toSign)
      let messedKeys = correctlySignedKeys & ix 3 . _2 .~ wrongSignature
      (msig, _) <- originate admin teamPks []
      lCall msig $
        Multisig.Parameter
          { stakerParam = STKR.NewCouncil newCouncilKeys
          , nonce = 1
          , signatures = messedKeys
          }
      validate . Left $
        lExpectCustomError #invalidSignature pk4

  it "fail if majority quorum of team keys is not reached" $
    integrationalTestExpectation $ do
      let teamPks = [pk1, pk2, pk3, pk4, pk5]
      let newCouncilKeys = [pk6, pk7]
      let toSign = ((dummyChainId, 1 :: Natural), STKR.NewCouncil newCouncilKeys)
      let correctlySignedKeys = multisignValue [sk2, sk4] toSign
      (msig, _) <- originate admin teamPks []
      lCall msig $
        Multisig.Parameter
          { stakerParam = STKR.NewCouncil newCouncilKeys
          , nonce = 1
          , signatures = correctlySignedKeys
          }
      validate . Left $
        lExpectCustomError #quorumNotReached ()
