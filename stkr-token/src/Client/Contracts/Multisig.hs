module Client.Contracts.Multisig
  ( DeployOptions (..)
  , deploy

  , CallOptions (..)
  , call
  ) where

import Prelude

import Tezos.Address (Address)
import Tezos.Core (unsafeMkMutez)
import Tezos.Crypto (KeyHash)

import Client.Tezos (TzEnv)
import qualified Client.Tezos as Tz

import qualified Lorentz.Contracts.Multisig as Msig

data DeployOptions = DeployOptions
  { contractAlias :: Text
  , originator :: Address
  , teamKeys :: Set KeyHash
  }

deploy :: DeployOptions -> TzEnv Address
deploy DeployOptions{..} = do
  let initStorage =
        Msig.Storage
          { currentNonce = 0
          , ..
          }
  Tz.originateContract $ Tz.OriginateContractP
    { ocpAlias = contractAlias
    , ocpQty = 0
    , ocpSrc = originator
    , ocpContract = Msig.multisigContract
    , ocpInitalStorage = initStorage
    , ocpBurnCap = 200
    }


data CallOptions = CallOptions
  { caller :: Address
  , contract :: Address
  , parameter :: Msig.Parameter
  }
call :: CallOptions -> TzEnv ()
call CallOptions{..} = Tz.transfer $
  Tz.TransferP
    { tpQty = unsafeMkMutez 0
    , tpSrc = caller
    , tpDst = contract
    , tpBurnCap = 22
    , tpArgument = parameter
    }
