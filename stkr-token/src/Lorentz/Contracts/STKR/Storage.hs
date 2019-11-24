module Lorentz.Contracts.STKR.Storage
  ( Storage(..)
  , Hash
  , URL
  ) where

import Fmt (Buildable(..), Builder, blockMapF, jsonListF, mapF', base64F, (+|), (|+))

import Lorentz

type Hash = ByteString
type URL = MText

data Storage = Storage
  { owner :: Address
  , team :: Maybe Address
  , councilKeys :: [PublicKey] -- TODO: Ahh, public keys are not comparible as I see
  , urls :: Map MText (Hash, URL)
  }
  deriving stock Generic
  deriving anyclass IsoValue

instance Buildable Storage where
  build Storage{..} = blockMapF @[(Text, Builder)] $
    [ ("owner", build owner)
    , ("team", build team)
    , ("councilKeys", jsonListF councilKeys)
    , ("urls",
       mapF' build (
          \(hash, value) ->
            "(" +| base64F hash |+ "," +| build value |+ ""
          ) urls
      )
    ]
