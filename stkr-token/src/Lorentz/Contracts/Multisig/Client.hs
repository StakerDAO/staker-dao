module Lorentz.Contracts.Multisig.Client
  ( DeployOptions (..)
  , deploy
  ) where

import Prelude

import Tezos.Address (Address)
import Tezos.Crypto (KeyHash)

import TzTest (TzTest)
import qualified TzTest as Tz

import qualified Lorentz.Contracts.Multisig as Multisig

data DeployOptions = DeployOptions
  { contractAlias :: Text
  , originator :: Address
  , teamKeys :: Set KeyHash
  }

deploy :: DeployOptions -> TzTest Address
deploy DeployOptions{..} = do
  let initStorage =
        Multisig.Storage
          { currentNonce = 0
          , teamKeys = teamKeys
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
