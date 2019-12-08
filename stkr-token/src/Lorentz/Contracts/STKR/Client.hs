module Lorentz.Contracts.STKR.Client
  ( DeployOptions(..)
  , deploy

  , getStorage

  , CallOptions(..)
  , call
  ) where

import Prelude

import qualified Data.Map.Strict as Map

import Tezos.Address (Address)
import Tezos.Core (unsafeMkMutez)
import Tezos.Crypto (PublicKey)

import qualified Lorentz.Contracts.STKR as STKR
import TzTest (TzTest)
import qualified TzTest as Tz

data DeployOptions = DeployOptions
  { contractAlias :: Text
  , originator :: Address
  , councilPks :: [PublicKey]
  , teamMultisig :: Address
  }

deploy :: DeployOptions -> TzTest Address
deploy DeployOptions{..} = do
  let initStorage =
        STKR.Storage
          { owner = teamMultisig
          , councilKeys = councilPks
          , urls = Map.empty
          }
  Tz.originateContract $
    Tz.OriginateContractP
      { ocpAlias = contractAlias
      , ocpQty = 0
      , ocpSrc = originator
      , ocpContract = STKR.stkrContract
      , ocpInitalStorage = initStorage
      , ocpBurnCap = 220
      }

getStorage :: Address -> TzTest STKR.Storage
getStorage = Tz.getStorage @STKR.Storage

data CallOptions = CallOptions
  { caller :: Address
  , contract :: Address
  , parameter :: STKR.Parameter
  }

call
  :: CallOptions
  -> TzTest ()
call CallOptions{..} = Tz.transfer $
  Tz.TransferP
    { tpQty = unsafeMkMutez 0
    , tpSrc = caller
    , tpDst = contract
    , tpBurnCap = 22
    , tpArgument = parameter
    }
