{-# LANGUAGE NoRebindableSyntax #-}

module Lorentz.Contracts.STKR.Governance.TypeDefs
  ( Hash
  , URL
  , Blake2BHash(..)
  , blake2B_
  , Policy
  , Proposal
  , proposalText2Proposal
  , ProposalAndHash
  , VoteForProposalParams
  , CouncilDataToSign(..)
  , TimeConfig(..)
  ) where

import Util.Named ((:!))
import Prelude (Show, Traversable(..), (<$>))
import Data.Map as M
import Text.Hex (decodeHex)
import Data.Aeson (FromJSON)

import Lorentz
import Michelson.Text (mkMTextUnsafe)

type Hash = ByteString
type URL = MText

newtype Blake2BHash = Blake2BHash ByteString
  deriving newtype (IsoValue, Show, Eq)

blake2B_ :: ByteString & s :-> Blake2BHash & s
blake2B_ = blake2B # coerce_

type Policy =
  ( "urls" :! Map MText (Hash, URL)
  )

type Proposal =
  ( "description" :! MText
  , "newPolicy" :! Policy
  )

-- This is needed to define clean `FromJSON` instance,
--   consider simplifying
data ProposalText = ProposalText {
    ptDescription :: Text
  , ptNewPolicy :: Map Text (Text, Text)
  } deriving (Generic, FromJSON)

proposalText2Proposal :: ProposalText -> Maybe Proposal
proposalText2Proposal ProposalText{..} = do
    decoded <- traverse decodeHU ptNewPolicy
    pure 
      ( #description $ mkMTextUnsafe ptDescription
      , #newPolicy $ #urls $ M.mapKeys mkMTextUnsafe decoded)
  where
    decodeHU (hash, url) = (, mkMTextUnsafe url) <$> decodeHex hash

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
  | ProdTC { _firstYear :: Natural }
