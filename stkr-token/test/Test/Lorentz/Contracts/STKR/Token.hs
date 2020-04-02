module Test.Lorentz.Contracts.STKR.Token
  ( spec_Transfer
  ) where

import Prelude

import CryptoInterop (toPublic)
import Lens.Micro (at)
import Lorentz (unTAddress)
import Lorentz.Test
import Test.Hspec (Spec, it)
import Util.Named ((.!))

import qualified Lorentz.Contracts.STKR as STKR

import Test.Lorentz.Contracts.STKR.Common
  (OriginateParams(..), callWithMultisig, failWhenNot, newKeypair, originate,
  wallet1, wallet2)

spec_Transfer :: Spec
spec_Transfer = do
  let (sk1, _) = newKeypair "1"
  let (sk2, _) = newKeypair "2"
  let (sk3, _) = newKeypair "3"
  let (sk4, _) = newKeypair "4"
  let (sk5, _) = newKeypair "5"
  let teamSks = [sk1, sk2, sk3, sk4, sk5]
  let teamPks = toPublic <$> teamSks
  let currentNonce = 1
  it "transfer tokens if operation is signed by team" . integrationalTestExpectation $ do
    let transferParam = (#from .! wallet1, #to .! wallet2, #value .! 100)
    (msig, stkr) <- originate $ OriginateParams
      { opTeamKeys = teamPks
      , opCouncilKeys = []
      , opInitailLedger = [(wallet1, 150)]
      }
    callWithMultisig msig currentNonce teamSks stkr $ STKR.Transfer transferParam
    validate . Right . lExpectStorageUpdate stkr $ \STKR.Storage{..} -> do
      failWhenNot (ledger ^. at wallet1 == Just 50) "Withraw from wallet1 not equal to 100"
      failWhenNot (ledger ^. at wallet2 == Just 100) "Credit to wallet2 not equal to 100"

  it "transfer removes wallet from ledger if its balance drops to zero"
    . integrationalTestExpectation $ do
      let transferParam = (#from .! wallet1, #to .! wallet2, #value .! 150)
      (msig, stkr) <- originate $ OriginateParams
        { opTeamKeys = teamPks
        , opCouncilKeys = []
        , opInitailLedger = [(wallet1, 150)]
        }
      callWithMultisig msig currentNonce teamSks stkr $ STKR.Transfer transferParam
      validate . Right . lExpectStorageUpdate stkr $ \STKR.Storage{..} -> do
        failWhenNot (ledger ^. at wallet1 == Nothing) "Wallet still present in ledger"
        failWhenNot (ledger ^. at wallet2 == Just 150) "Credit to wallet2 not equal to 150"

  it "fail to transfer token if the operation is not signed"
    . integrationalTestExpectation $ do
      (msig, stkr) <- originate $ OriginateParams
        { opTeamKeys = teamPks
        , opCouncilKeys = []
        , opInitailLedger = [(wallet1, 150)]
        }
      lCallDef stkr
        $ STKR.OpsTeamEntrypoint
        . STKR.EnsureOwner
        . STKR.Transfer $ (#from .! wallet1, #to .! wallet2, #value .! 100)
      validate . Left $
        lExpectCustomError #senderCheckFailed (unTAddress msig)
