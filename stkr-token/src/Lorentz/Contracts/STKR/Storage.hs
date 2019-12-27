{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.STKR.Storage
  ( Storage(..)
  ) where

import Lorentz

import Fmt (Buildable(..), Builder, blockMapF, jsonListF, mapF', base64F, (+|), (|+))
import Tezos.Crypto (formatKeyHash)

import Lorentz.Contracts.STKR.Governance.TypeDefs
  (Proposal, ProposalAndHash, ProposalId, Hash, URL, Policy)
import Lorentz.Contracts.STKR.Parameter (PublicEntrypointParam(..))

data Storage = Storage
  { owner :: Address
  , councilKeys :: Set KeyHash
  , policy :: Policy
  , proposals :: Map ProposalId ProposalAndHash
  , lastProposalId :: Natural
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
    , ("totalSupply", build totalSupply)
    , ("ledger", "BigMap values should not be displayed")
    ]
