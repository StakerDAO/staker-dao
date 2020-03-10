{-# LANGUAGE RebindableSyntax #-}

module Test.Lorentz.Contracts.STKR.Upgradability
  ( spec_FreezeEntrypoint
  , spec_Successor
  ) where

import Prelude

import Lorentz hiding (lambda, (>>))
import qualified Lorentz as L
import Lorentz.Base (( # ))
import Lorentz.Contracts.Consumer
import Lorentz.Test
import Test.Hspec (Spec, it)
import qualified Test.Hspec.QuickCheck as HQ
import Tezos.Crypto (SecretKey, toPublic)
import Util.Named ((.!))

import qualified Lorentz.Contracts.Multisig as Multisig
import qualified Lorentz.Contracts.STKR as STKR

import Test.Lorentz.Contracts.STKR.Common
  (OriginateParams(..), callWithMultisig, callWithMultisig', expectSuccess,
  failWhenNot, newKeypair, originate, originateWithEmptyLedger, wallet1)

callSetSuccessor
  :: ContractRef Multisig.Parameter
  -> Natural
  -> [SecretKey]
  -> ContractRef STKR.Parameter
  -> Lambda STKR.PublicEntrypointParam Operation
  -> IntegrationalScenarioM ()
callSetSuccessor msig nonce teamSecretKeys stkr lambda = do
  callWithMultisig' msig #cPermitOnFrozen nonce teamSecretKeys stkr $
    STKR.SetSuccessor (#successor .! lambda)

spec_FreezeEntrypoint :: Spec
spec_FreezeEntrypoint = do
  let (sk1, _) = newKeypair "1"
  let (sk2, _) = newKeypair "2"
  let (sk3, _) = newKeypair "3"
  let (sk4, _) = newKeypair "4"
  let (sk5, _) = newKeypair "5"
  let teamSks = [sk1, sk2, sk3, sk4, sk5]
  let teamPks = toPublic <$> teamSks
  it "freeze entrypoints set frozen bool in storage" . integrationalTestExpectation $ do
    (msig, stkr) <- originateWithEmptyLedger teamPks []
    callWithMultisig msig 1 teamSks stkr $ STKR.Freeze ()
    validate . Right . lExpectStorageUpdate stkr $
      \STKR.Storage{..} -> failWhenNot frozen "Storage field haven't updated"

  it "fund entrypoint fails if contract is frozen" . integrationalTestExpectation $ do
    (msig, stkr) <- originateWithEmptyLedger teamPks []
    callWithMultisig msig 1 teamSks stkr $ STKR.Freeze ()

    lTransfer (#from .! genesisAddress) (#to .! stkr) (toMutez 100) $
      STKR.PublicEntrypoint $ STKR.Fund "dummybytestring"

    validate . Left $
      lExpectCustomError #contractFrozen ()

  HQ.prop "fails when opeations team entrypoint is called on frozen contract" $
    \(opsParam :: STKR.OpsTeamEntrypointParam) -> integrationalTestProperty $ do
      (msig, stkr) <- originate $ OriginateParams
        { opTeamKeys = teamPks
        , opCouncilKeys = []
        , opInitailLedger = [(wallet1, 150)]
        }
      callWithMultisig msig 1 teamSks stkr $ STKR.Freeze ()

      callWithMultisig msig 2 teamSks stkr opsParam

      validate . Left $
        lExpectCustomError #contractFrozen ()

  it "getTotalSupply call fails if contract is frozen" $
    integrationalTestExpectation $ do
      (msig, stkr) <- originateWithEmptyLedger teamPks []
      consumer <- lOriginateEmpty contractConsumer "consumer"

      callWithMultisig msig 1 teamSks stkr $ STKR.Freeze ()
      lCall stkr
        . STKR.PublicEntrypoint
        . STKR.GetTotalSupply $ L.mkView () consumer

      validate . Left $
        lExpectCustomError #contractFrozen ()

spec_Successor :: Spec
spec_Successor = do
  let (sk1, pk1) = newKeypair "1"
  let (sk2, pk2) = newKeypair "2"
  let (sk3, _) = newKeypair "3"
  let (sk4, _) = newKeypair "4"
  let (sk5, _) = newKeypair "5"
  let teamSks = [sk1, sk2, sk3, sk4, sk5]
  let teamPks = toPublic <$> teamSks

  it "cannot set successor to active contract" $
    integrationalTestExpectation $ do
      (msig, stkr) <- originateWithEmptyLedger teamPks []
      let successorLambda = STKR.successorLambda stkr
      callSetSuccessor msig 1 teamSks stkr successorLambda
      validate . Left $ lExpectCustomError #contractActive ()

  it "set successor lambda to storage" $
    integrationalTestExpectation $ do
      (msig, stkr) <- originateWithEmptyLedger teamPks []
      let successorLambda = STKR.successorLambda stkr
      callWithMultisig msig 1 teamSks stkr $ STKR.Freeze ()
      callSetSuccessor msig 2 teamSks stkr successorLambda
      validate . Right . lExpectStorageUpdate stkr $ \STKR.Storage{..} ->
        failWhenNot (successor == Just successorLambda) "Storage field haven't updated"

  it "set successor lambda to storage even after one already present" $
    integrationalTestExpectation $ do
      (msig, stkr) <- originateWithEmptyLedger teamPks []
      let successorLambda1 = L.push (1 :: Natural) # L.drop # STKR.successorLambda stkr
      let successorLambda2 = L.push (2 :: Natural) # L.drop # STKR.successorLambda stkr

      validate . Right . expectSuccess $
        failWhenNot (successorLambda1 /= successorLambda2) "Lamdas are equal"
      callWithMultisig msig 1 teamSks stkr $ STKR.Freeze ()

      callSetSuccessor msig 2 teamSks stkr successorLambda1
      validate . Right . lExpectStorageUpdate stkr $ \STKR.Storage{..} ->
        failWhenNot (successor == Just successorLambda1) "Storage field haven't updated"

      callSetSuccessor msig 3 teamSks stkr successorLambda2
      validate . Right . lExpectStorageUpdate stkr $ \STKR.Storage{..} ->
        failWhenNot (successor == Just successorLambda2) "Storage field haven't updated"

  it "executes successor lambda when public entrypoint is called" $
    integrationalTestExpectation $ do
      let valueOnStkr1 = 100
      let valueOnStkr2 = 150
      let teamKeys1 = [pk1]
      let teamKeys2 = [pk2]
      (msig1, stkr1) <- originate $ OriginateParams
        { opCouncilKeys = []
        , opTeamKeys = teamKeys1
        , opInitailLedger =  [(wallet1, valueOnStkr1)]
        }
      (_, stkr2) <- originate $ OriginateParams
        { opCouncilKeys = []
        , opTeamKeys = teamKeys2
        , opInitailLedger = [(wallet1, valueOnStkr2)]
        }

      callWithMultisig msig1 1 [sk1] stkr1 $ STKR.Freeze ()
      callSetSuccessor msig1 2 [sk1] stkr1 $ STKR.successorLambda stkr2
      consumer <- lOriginateEmpty @Natural contractConsumer "consumer"
      lCall stkr1
        . STKR.PublicEntrypoint
        . STKR.GetBalance
        $ L.mkView (#owner .! wallet1) consumer

      validate . Right . lExpectConsumerStorage consumer $ \consStorage ->
        failWhenNot (consStorage == [valueOnStkr2]) "Call not forwarded to successor"

  HQ.prop "fails when non public entrypoint is called on contract with successor" $
    \(opsParam :: STKR.OpsTeamEntrypointParam) -> integrationalTestProperty $ do
      (msig, stkr) <- originateWithEmptyLedger teamPks []
      let successorLambda = STKR.successorLambda stkr
      callWithMultisig msig 1 teamSks stkr $ STKR.Freeze ()
      callSetSuccessor msig 2 teamSks stkr successorLambda
      callWithMultisig msig 3 teamSks stkr opsParam
      validate . Left $ lExpectCustomError #contractFrozen ()
