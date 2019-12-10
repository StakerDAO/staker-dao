{-# OPTIONS_GHC -Wno-orphans #-}
module Lorentz.Contracts.STKR.Storage
  ( Storage(..)
  , Hash
  , URL
  , Proposal
  , Policy
  , Blake2BHash (..)
  , blake2B_
  ) where

import Fmt (Buildable(..), Builder, blockMapF, jsonListF, mapF', base64F, (+|), (|+))
import Tezos.Crypto (formatKeyHash)
import Lorentz

type Hash = ByteString
type URL = MText

newtype Blake2BHash = Blake2BHash ByteString
  deriving newtype IsoValue

blake2B_ :: ByteString & s :-> Blake2BHash & s
blake2B_ = blake2B # coerce_

type Policy =
  ( "urls" :! Map MText (Hash, URL)
  )

type Proposal =
  ( "description" :! MText
  , "newPolicy" :! Policy
  )

type ProposalAndHash = ("proposal" :! Proposal, "proposalHash" :! Blake2BHash)

data Storage = Storage
  { owner :: Address
  , councilKeys :: Set KeyHash
  , policy :: Policy
  , proposals :: [ProposalAndHash]
  , votes :: Map KeyHash ("proposalId" :! Natural)
  , stageCounter :: Natural
  -- ^ @stageCounter `div` 4@ is current epoch and @stageCounter `mod` 4@
  -- denotes current stage within an epoch
  }
  deriving stock Generic
  deriving anyclass IsoValue

instance Buildable (Map MText (Hash, URL)) where
  build urls =
    mapF' build (
        \(hash, value) ->
          "(" +| base64F hash |+ "," +| build value |+ ""
        ) urls

instance Buildable Proposal where
  build (desc, newPolicy) = build desc |+ ": " +| build newPolicy

instance Buildable ProposalAndHash where
  build (proposal, _) = build proposal

instance Buildable Storage where
  build Storage{..} = blockMapF @[(Text, Builder)] $
    [ ("owner", build owner)
    , ("councilKeys", jsonListF councilKeys)
    , ("policy", build policy)
    , ("proposals", jsonListF proposals)
    , ("votes", mapF' (build . formatKeyHash) build votes)
    , ("stageCounter", build stageCounter)
    ]
