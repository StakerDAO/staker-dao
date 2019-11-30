module Lorentz.Contracts.Multisig.Client
  ( DeployOptions (..)
  , deploy
  ) where

import Prelude

import GHC.Natural (intToNatural)

import qualified Data.Set as Set
import Tezos.Address (Address)
import Tezos.Crypto (KeyHash)

import TzTest (TzTest)
import qualified TzTest as Tz

import qualified Lorentz.Contracts.Multisig as Multisig

data DeployOptions = DeployOptions
  { contractAlias :: Text
  , originator :: Address
  , keys :: Set KeyHash
  , stakerAddress :: Address
  }

deploy :: DeployOptions -> TzTest Address
deploy DeployOptions{..} = do
  let initStorage =
        Multisig.Storage
          { quorum = intToNatural $ (Set.size keys `div` 2) + 1
          , currentNonce = 0
          , ..
          }
  Tz.originateContract $ Tz.OriginateContractP
    { ocpAlias = contractAlias
    , ocpQty = 0
    , ocpSrc = originator
    , ocpContract = Multisig.multisigContract
    , ocpInitalStorage = initStorage
    , ocpBurnCap = 200
    }
