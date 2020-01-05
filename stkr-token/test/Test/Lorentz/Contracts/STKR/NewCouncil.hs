module Test.Lorentz.Contracts.STKR.NewCouncil
  ( spec_NewCouncil
  ) where

import Prelude

import qualified Data.Set as S
import Fmt (listF, (+|), (|+))
import Lorentz (fromContractAddr)
import Lorentz.Test
import Michelson.Test.Dummy (dummyNow)
import Test.Hspec (Spec, it)
import Tezos.Core (timestampPlusSeconds)
import Tezos.Crypto (detSecretKey, hashKey, toPublic)

import qualified Lorentz.Contracts.STKR as STKR

import Test.Lorentz.Contracts.STKR.Common (callWithMultisig, originateWithEmptyLedger)


spec_NewCouncil :: Spec
spec_NewCouncil = newCouncilSpec

newCouncilSpec :: Spec
newCouncilSpec = do
  let teamSks = detSecretKey <$> ["1", "2", "3", "4", "5"]
  let teamPks = toPublic <$> teamSks
  let newCouncilKeys = S.fromList $ (hashKey . toPublic . detSecretKey) <$> ["6", "7"]

  it "updates council keys if called via multisig" $
    integrationalTestExpectation $ do
      (msig, stkr) <- originateWithEmptyLedger teamPks []

      callWithMultisig msig 1 teamSks stkr $ STKR.NewCouncil newCouncilKeys

      validate . Right . lExpectStorageUpdate stkr $ \storage ->
        if newCouncilKeys == (STKR.councilKeys storage)
        then pass
        else Left . CustomValidationError $
               "Expected " +| listF newCouncilKeys |+
               ", but got " +| listF (STKR.councilKeys storage) |+ ""

  it "fails to update council keys if called on stage 3" $
    integrationalTestExpectation $ do
      (msig, stkr) <- originateWithEmptyLedger teamPks []
      waitForStage 2  -- Stage numbering starts from zero

      callWithMultisig msig 1 teamSks stkr $ STKR.NewCouncil newCouncilKeys

      validate . Left $
        lExpectCustomError #wrongStage (#stageCounter 2)

  it "fails to update council keys if called on stage 4" $
    integrationalTestExpectation $ do
      (msig, stkr) <- originateWithEmptyLedger teamPks []
      waitForStage 3  -- Stage numbering starts from zero

      callWithMultisig msig 1 teamSks stkr $ STKR.NewCouncil newCouncilKeys

      validate . Left $
        lExpectCustomError #wrongStage (#stageCounter 3)

  it "fails if called directly" $
    integrationalTestExpectation $ do
      (msig, stkr) <- originateWithEmptyLedger teamPks []

      lCall stkr $
        STKR.OpsTeamEntrypoint .
        STKR.EnsureOwner .
        STKR.NewCouncil $ newCouncilKeys
      validate . Left $
        lExpectCustomError #senderCheckFailed (fromContractAddr msig)

  where
    waitForStage n = setNow $ timestampPlusSeconds dummyNow n
