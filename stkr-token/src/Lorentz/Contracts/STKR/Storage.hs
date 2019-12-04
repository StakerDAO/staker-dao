{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lorentz.Contracts.STKR.Storage
  ( Storage(..)
  ) where

import Fmt (Buildable(..), Builder, blockMapF, jsonListF, mapF', base64F, (+|), (|+))

import Lorentz

import Lorentz.Contracts.STKR.Types (Hash, URL)
import Lorentz.Contracts.STKR.Proposal (BookState, Proposal)

data Storage = Storage
  { owner :: Address
  , team :: Maybe Address
  , councilKeys :: [PublicKey] -- TODO: Ahh, public keys are not comparible as I see
  , urls :: Map MText (Hash, URL)
  , currentBook :: BookState
  , proposals {- ??? -} :: [(Proposal, {- Map -} [(PublicKey, Signature)])] -- List of pairs instead of Map, see above
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
