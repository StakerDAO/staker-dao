{-# LANGUAGE RebindableSyntax #-}

module Test.Lorentz.Contracts.STKR.Upgradability
  ( spec_FreezeEntrypoint
  , spec_Successor
  ) where

import Prelude hiding (drop)

import qualified Data.Map as Map

import Lorentz hiding ((>>))
import Lorentz.Contracts.Consumer
import Lorentz.Test
import Test.Hspec (Spec, describe, it)
import Tezos.Crypto (toPublic)
import Util.Named ((.!))
import Text.Hex (decodeHex)

import qualified Lorentz.Contracts.STKR as STKR

import Test.Lorentz.Contracts.STKR.Common
  (OriginateParams(..), callSetSuccessor, callWithMultisig, failWhenNot,
  newKeypair, originate, originateWithEmptyLedger, wallet1, wallet2, expectSuccess)

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

  -- TODO: looks like property test, but don't want to mess with it for now
  describe "all Public and OpsTeam entrypoint fails when contract is frozen" $ do
    it "fund entrypoint fails if contract is frozen" . integrationalTestExpectation $ do
      (msig, stkr) <- originateWithEmptyLedger teamPks []
      callWithMultisig msig 1 teamSks stkr $ STKR.Freeze ()

      lTransfer (#from .! genesisAddress) (#to .! stkr) (toMutez 100) $
        STKR.PublicEntrypoint $ STKR.Fund "dummybytestring"

      validate . Left $
        lExpectCustomError #contractFrozen ()

    it "newProposal call fails if contract is frozen" . integrationalTestExpectation $ do
      (msig, stkr) <- originate $ OriginateParams
        { opTeamKeys = teamPks
        , opCouncilKeys = []
        , opInitailLedger = [(wallet1, 150)]
        }
      callWithMultisig msig 1 teamSks stkr $ STKR.Freeze ()

      let hash = fromMaybe (error "tezosWpUrlHash: unexpected") . decodeHex $
            "be7663e0ef87d51ab149a60dfad4df5940d30395ba287d9907f8d66ce5061d96"
      let url = [mt|https://tezos.com/static/white_paper-2dc8c02267a8fb86bd67a108199441bf.pdf|]
      let tezosWpUrlHash = (hash, url)
      let newUrls = Map.singleton [mt|"tezos-wp|] tezosWpUrlHash
      let newProposal =
            ( #description [mt|"First"|]
            , #newPolicy (#urls newUrls)
            )
      callWithMultisig msig 2 teamSks stkr $ STKR.NewProposal newProposal

      validate . Left $
        lExpectCustomError #contractFrozen ()

    it "transfer call fails if contract is frozen" $
      integrationalTestExpectation $ do
        let transferParam = (#from .! wallet1, #to .! wallet2, #value .! 100)
        (msig, stkr) <- originate $ OriginateParams
          { opTeamKeys = teamPks
          , opCouncilKeys = []
          , opInitailLedger = [(wallet1, 150)]
          }
        callWithMultisig msig 1 teamSks stkr $ STKR.Freeze ()
        callWithMultisig msig 2 teamSks stkr $ STKR.Transfer transferParam
        validate . Left $
          lExpectCustomError #contractFrozen ()

    it "getTotalSupply call fails if contract is frozen"
      . integrationalTestExpectation $ do
        (msig, stkr) <- originateWithEmptyLedger teamPks []
        consumer <- lOriginateEmpty contractConsumer "consumer"

        callWithMultisig msig 1 teamSks stkr $ STKR.Freeze ()
        lCall stkr
          . STKR.PublicEntrypoint
          . STKR.GetTotalSupply $ mkView () consumer

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
      let successorLambda1 = push (1 :: Natural) # drop # STKR.successorLambda stkr
      let successorLambda2 = push (2 :: Natural) # drop # STKR.successorLambda stkr

      validate . Right . expectSuccess $
        failWhenNot (successorLambda1 /= successorLambda2) "Lamdas are equal"
      callWithMultisig msig 1 teamSks stkr $ STKR.Freeze ()

      callSetSuccessor msig 2 teamSks stkr successorLambda1
      validate . Right . lExpectStorageUpdate stkr $ \STKR.Storage{..} ->
        failWhenNot (successor == Just successorLambda1) "Storage field haven't updated"

      callSetSuccessor msig 3 teamSks stkr successorLambda2
      validate . Right . lExpectStorageUpdate stkr $ \STKR.Storage{..} ->
        failWhenNot (successor == Just successorLambda2) "Storage field haven't updated"

  -- TODO: looks like another property test
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
        $ mkView (#owner .! wallet1) consumer

      validate . Right . lExpectConsumerStorage consumer $ \consStorage ->
        failWhenNot (consStorage == [valueOnStkr2]) "Call not forwarded to successor"

  it "fails when non public entrypoint is called on contract with successor" $
    integrationalTestExpectation $ do
      (msig, stkr) <- originateWithEmptyLedger teamPks []
      let successorLambda = STKR.successorLambda stkr
      let transferParam = (#from .! wallet1, #to .! wallet2, #value .! 100)
      callWithMultisig msig 1 teamSks stkr $ STKR.Freeze ()
      callSetSuccessor msig 2 teamSks stkr successorLambda
      callWithMultisig msig 3 teamSks stkr $ STKR.Transfer transferParam
      validate . Left $ lExpectCustomError #contractFrozen ()
