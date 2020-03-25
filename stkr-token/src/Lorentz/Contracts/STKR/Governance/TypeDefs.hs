{-# LANGUAGE NoRebindableSyntax #-}

module Lorentz.Contracts.STKR.Governance.TypeDefs
  ( Sha256Hash (..)
  , URL
  , Blake2BHash (..)
  , blake2B_
  , Policy
  , Proposal
  , ProposalAndHash
  , VoteForProposalParams
  , CouncilDataToSign(..)
  , TimeConfig(..)
  ) where

import Data.Aeson (FromJSON(..))
import Data.Map as M
import Prelude (Show, fail, maybe, (>=>))
import Text.Hex (decodeHex)
import Util.Named ((:!))

import Lorentz

type URL = MText

newtype Sha256Hash = Sha256Hash ByteString
  deriving newtype (Show, Eq)
  deriving stock (Generic)
  deriving anyclass (HasTypeAnn)

instance IsoValue Sha256Hash

instance FromJSON Sha256Hash where
  parseJSON = parseJSON >=>
    maybe (fail "invalid sha256 hash")
      (pure . Sha256Hash) . decodeHex

newtype Blake2BHash = Blake2BHash ByteString
  deriving newtype (Show, Eq)
  deriving stock (Generic)

instance IsoValue Blake2BHash

blake2B_ :: ByteString & s :-> Blake2BHash & s
blake2B_ = blake2B # forcedCoerce_ -- TODO: use safe coercions

type Policy =
  ( "urls" :! Map MText (Sha256Hash, URL)
  )

type Proposal =
  ( "description" :! MText
  , "newPolicy" :! Policy
  )

type ProposalAndHash = ("proposal" :! Proposal, "proposalHash" :! Blake2BHash)

type VoteForProposalParams =
  ( "proposalId" :! Natural
  , "votePk" :! PublicKey
  , "voteSig" :! Signature
  )


data CouncilDataToSign = CouncilDataToSign
  { cdProposalHash :: Blake2BHash
  , cdContractAddr :: Address
  , cdStageCounter :: Natural
  }
  deriving stock Generic
  deriving anyclass IsoValue

data TimeConfig =
    TestTC { _start :: Timestamp
           , _stageDuration :: Natural
           }
  | ProdTC { _startYear :: Natural }
