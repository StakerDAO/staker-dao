module Lorentz.Contracts.Multisig.Storage
  ( Storage (..)
  ) where

import Lorentz

import Fmt (Buildable(..), Builder, blockMapF, jsonListF)

data Storage = Storage
  { teamKeys :: Set KeyHash
  , currentNonce :: Natural
  } deriving stock Generic
    deriving anyclass IsoValue

instance Buildable Storage where
    build Storage{..} = blockMapF @[(Text, Builder)] $
      [ ("teamKeys", jsonListF teamKeys)
      , ("currentNonce", build currentNonce)
      ]
