module Lorentz.Contracts.Client
  ( DeployOptions(..)
  , deploy
  ) where

import Prelude

import Fmt (Buildable(..), Builder, mapF)

import Tezos.Address (Address)
import Tezos.Crypto (KeyHash, PublicKey)

import TzTest (TzTest)

import qualified Lorentz.Contracts.Multisig.Client as Msig
import qualified Lorentz.Contracts.STKR as STKR
import qualified Lorentz.Contracts.STKR.Client as STKR

data DeployOptions = DeployOptions
  { msigAlias :: Text
  , tokenAlias :: Text
  , originator :: Address
  , councilPks :: [PublicKey]
  , teamKeys :: Set KeyHash
  }

data DeployResult = DeployResult
  { tokenAddr :: Address
  , msigAddr :: Address
  }

instance Buildable DeployResult where
  build DeployResult{..} = mapF @[(Text, Builder)] $
    [ ("token", build tokenAddr)
    , ("multisig", build msigAddr)
    ]

deploy :: DeployOptions -> TzTest DeployResult
deploy DeployOptions{..} = do
  tokenAddr <- STKR.deploy $ STKR.DeployOptions
    { contractAlias = tokenAlias
    , ..
    }
  msigAddr <- Msig.deploy $ Msig.DeployOptions
    { contractAlias = msigAlias
    , keys = teamKeys
    , stakerAddress = tokenAddr
    , ..
    }
  STKR.call $ STKR.CallOptions
    { caller = originator
    , contract = tokenAddr
    , parameter = STKR.SetOperationsTeam msigAddr
    }
  pure DeployResult{..}
