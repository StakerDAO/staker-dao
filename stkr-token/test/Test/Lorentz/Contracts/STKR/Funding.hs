{-# LANGUAGE RebindableSyntax #-}

module Test.Lorentz.Contracts.STKR.Funding
  ( spec_Fund
  , spec_Withdraw
  ) where

import Prelude hiding (drop)

import Lorentz hiding ((>>))
import Lorentz.CryptoInterop (SecretKey)
import Lorentz.Test
import Test.Hspec (Spec, it)
import Tezos.Address (parseAddress)
import Util.Named ((.!))

import qualified Lorentz.Contracts.Multisig as Multisig
import qualified Lorentz.Contracts.STKR as STKR

import Test.Lorentz.Contracts.STKR.Common
  (callWithMultisig', mkTeamKeys, originateWithEmptyLedger)

callWithdraw
  :: TAddress Multisig.Parameter
  -> Natural
  -> [SecretKey]
  -> TAddress STKR.Parameter
  -> STKR.WithdrawParams
  -> IntegrationalScenarioM ()
callWithdraw msig nonce teamSecretKeys stkr param = do
  callWithMultisig' msig #cPermitOnFrozen nonce teamSecretKeys stkr $
    STKR.Withdraw param

receiver :: Address
receiver = either (error "unexpected: could not parse tz1 address") id $
  parseAddress "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"

spec_Fund :: Spec
spec_Fund = do
  let (_, teamPks) = unzip mkTeamKeys

  it "fund entrypoint accepts funding" . integrationalTestExpectation $ do
    (_, stkr) <- originateWithEmptyLedger teamPks []

    let stkrAddress = unTAddress stkr
    lTransfer (#from .! genesisAddress) (#to .! stkr) (toMutez 100) CallDefault $
        STKR.PublicEntrypoint $ STKR.Fund "dummybytestring"
    validate . Right $ lExpectBalance stkrAddress (toMutez 100)


spec_Withdraw :: Spec
spec_Withdraw = do
  let (teamSks, teamPks) = unzip mkTeamKeys

  it "succeeds if called by owner" . integrationalTestExpectation $ do
    (msig, stkr) <- originateWithEmptyLedger teamPks []

    let stkrAddress = unTAddress stkr
    lTransfer (#from .! genesisAddress) (#to .! stkr) (toMutez 100) CallDefault $
        STKR.PublicEntrypoint $ STKR.Fund "dummybytestring"

    callWithdraw msig 1 teamSks stkr (#to .! receiver, #amount .! toMutez 50)

    validate . Right $ lExpectBalance stkrAddress (toMutez 50)
    validate . Right $ lExpectBalance receiver (toMutez 50)

  it "fails if called by a non-owner" . integrationalTestExpectation $ do
    (msig, stkr) <- originateWithEmptyLedger teamPks []

    lTransfer (#from .! genesisAddress) (#to .! stkr) (toMutez 100) CallDefault $
        STKR.PublicEntrypoint $ STKR.Fund "dummybytestring"

    let withdraw = STKR.PermitOnFrozen . STKR.EnsureOwner . STKR.Withdraw
    lCallDef stkr $ withdraw (#to .! receiver, #amount .! toMutez 50)
    validate . Left $ lExpectCustomError #senderCheckFailed (unTAddress msig)
