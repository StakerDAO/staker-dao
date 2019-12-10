module Lorentz.Contracts.Client
  ( DeployOptions(..)
  , deploy
  , multisignValue
  ) where

import Prelude

import Fmt (Buildable(..), Builder, mapF)

import Lorentz.Constraints (NicePackedValue)
import Lorentz.Pack (lPackValue)
import Tezos.Address (Address)
import Tezos.Crypto (KeyHash, PublicKey, SecretKey, Signature, sign, toPublic)

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
  msigAddr <- Msig.deploy @STKR.Parameter $ Msig.DeployOptions
    { contractAlias = msigAlias
    , teamKeys = teamKeys
    , ..
    }
  tokenAddr <- STKR.deploy $ STKR.DeployOptions
    { contractAlias = tokenAlias
    , teamMultisig = msigAddr
    , ..
    }
  pure DeployResult{..}

multisignValue
  :: NicePackedValue a
  => [SecretKey] -- Sks to be signed with
  -> a           -- Value to be signed
  -> [(PublicKey, Signature)]
multisignValue opsSks newCouncil =
  let packedCouncil = lPackValue newCouncil
  in (\sk -> (toPublic sk, sign sk packedCouncil)) <$> opsSks