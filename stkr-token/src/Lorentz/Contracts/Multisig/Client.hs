module Lorentz.Contracts.Multisig.Client
  ( DeployOptions (..)
  , deploy

  , CallOptions (..)
  , call
  ) where

import Prelude

import Lorentz.Constraints (NicePackedValue, NiceParameter, NicePrintedValue)

import Tezos.Address (Address)
import Tezos.Core (unsafeMkMutez)
import Tezos.Crypto (KeyHash)

import TzTest (TzTest)
import qualified TzTest as Tz

import qualified Lorentz.Contracts.Multisig as Msig

data DeployOptions = DeployOptions
  { contractAlias :: Text
  , originator :: Address
  , teamKeys :: Set KeyHash
  }

deploy
  :: forall a. (NicePackedValue a, NiceParameter a)
  => DeployOptions -> TzTest Address
deploy DeployOptions{..} = do
  let initStorage =
        Msig.Storage
          { currentNonce = 0
          , teamKeys = teamKeys
          , ..
          }
  Tz.originateContract $ Tz.OriginateContractP
    { ocpAlias = contractAlias
    , ocpQty = 0
    , ocpSrc = originator
    , ocpContract = Msig.multisigContract @a
    , ocpInitalStorage = initStorage
    , ocpBurnCap = 200
    }


data CallOptions a = CallOptions
  { caller :: Address
  , contract :: Address
  , parameter :: Msig.Parameter a
  }
call
  :: forall a. NicePrintedValue a
  => CallOptions a -> TzTest ()
call CallOptions{..} = Tz.transfer $
  Tz.TransferP
    { tpQty = unsafeMkMutez 0
    , tpSrc = caller
    , tpDst = contract
    , tpBurnCap = 22
    , tpArgument = parameter
    }
