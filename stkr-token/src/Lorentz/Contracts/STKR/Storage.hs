{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.STKR.Storage
  ( Storage(..)
  ) where

import Lorentz

import Data.Functor.Identity (Identity (..))
import Named (NamedF (..))

import Fmt (Buildable(..), Builder, blockMapF, jsonListF, mapF', base64F, (+|), (|+))
import Tezos.Crypto (formatKeyHash, KeyHash (..))

import Lorentz.Contracts.STKR.Governance.TypeDefs (Blake2BHash (..), Proposal, ProposalAndHash, Sha256Hash (..), URL, Policy)
import Lorentz.Contracts.STKR.Parameter (PublicEntrypointParam(..))

data Storage = Storage
  { owner :: Address
  , councilKeys :: Set KeyHash
  , policy :: Policy
  , proposals :: [ProposalAndHash]
  , votes :: Map KeyHash ("proposalId" :! Natural)
  , stageCounter :: Natural
  -- ^ @stageCounter `div` 4@ is current epoch and @stageCounter `mod` 4@
  -- denotes current stage within an epoch
  , totalSupply :: Natural
  , ledger :: BigMap Address Natural
  , frozen :: Bool
  , successor :: Maybe (Lambda PublicEntrypointParam Operation)
  }
  deriving stock Generic
  deriving anyclass IsoValue

instance Buildable (Map MText (Sha256Hash, URL)) where
  build urls =
    mapF' build (
        \(Sha256Hash hash, value) ->
          "(" +| base64F hash |+ "," +| build value |+ ""
        ) urls

instance Buildable Proposal where
  build (desc, newPolicy) = build desc |+ ": " +| build newPolicy

instance Buildable ProposalAndHash where
  build (proposal, ArgF (Identity (Blake2BHash hash))) =
    build proposal |+ " (hash " +| build (KeyHash hash) |+ ")"

instance Buildable Storage where
  build Storage{..} = blockMapF @[(Text, Builder)] $
    [ ("owner", build owner)
    , ("councilKeys", jsonListF councilKeys)
    , ("policy", build policy)
    , ("proposals", jsonListF proposals)
    , ("votes", mapF' (build . formatKeyHash) build votes)
    , ("stageCounter", build stageCounter)
    , ("totalSupply", build totalSupply)
    , ("ledger", "BigMap values should not be displayed")
    ]
