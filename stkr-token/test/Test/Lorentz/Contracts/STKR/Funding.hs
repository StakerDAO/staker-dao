{-# LANGUAGE RebindableSyntax #-}

module Test.Lorentz.Contracts.STKR.Funding
  ( spec_Fund
  , spec_Withdraw
  ) where

import Prelude hiding (drop)

import Lorentz hiding ((>>))
import Lorentz.Test
import Test.Hspec (Spec, it)
import Tezos.Address (parseAddress)
import Util.Named ((.!))

import qualified Lorentz.Contracts.STKR as STKR

import Test.Lorentz.Contracts.STKR.Common
  (callWithdraw, mkTeamKeys, originateWithEmptyLedger)

receiver :: Address
receiver = either (error "unexpected: could not parse tz1 address") id $
  parseAddress "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"

spec_Fund :: Spec
spec_Fund = do
  let (_, teamPks) = unzip mkTeamKeys

  it "fund entrypoint accepts funding" . integrationalTestExpectation $ do
    (_, stkr) <- originateWithEmptyLedger teamPks []

    let stkrAddress = fromContractAddr stkr :: FutureContract STKR.Parameter
    lTransfer (#from .! genesisAddress) (#to .! stkr) (toMutez 100) $
        STKR.PublicEntrypoint $ STKR.Fund "dummybytestring"
    validate . Right $ lExpectBalance stkrAddress (toMutez 100)


spec_Withdraw :: Spec
spec_Withdraw = do
  let (teamSks, teamPks) = unzip mkTeamKeys

  it "succeeds if called by owner" . integrationalTestExpectation $ do
    (msig, stkr) <- originateWithEmptyLedger teamPks []

    let stkrAddress = fromContractAddr stkr :: FutureContract STKR.Parameter
    lTransfer (#from .! genesisAddress) (#to .! stkr) (toMutez 100) $
        STKR.PublicEntrypoint $ STKR.Fund "dummybytestring"

    callWithdraw msig 1 teamSks stkr (#to .! receiver, #amount .! toMutez 50)

    validate . Right $ lExpectBalance stkrAddress (toMutez 50)
    validate . Right $ lExpectBalance (FutureContract receiver) (toMutez 50)

  it "fails if called by a non-owner" . integrationalTestExpectation $ do
    (msig, stkr) <- originateWithEmptyLedger teamPks []

    lTransfer (#from .! genesisAddress) (#to .! stkr) (toMutez 100) $
        STKR.PublicEntrypoint $ STKR.Fund "dummybytestring"

    lCall stkr $ STKR.Withdraw $ STKR.EnsureOwner (#to .! receiver, #amount .! toMutez 50)
    validate . Left $ lExpectCustomError #senderCheckFailed (fromContractAddr msig)
