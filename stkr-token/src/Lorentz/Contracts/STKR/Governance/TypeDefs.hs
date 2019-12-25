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
import Prelude (Show)
import Data.Map as M
import Data.Text.Encoding (encodeUtf8)
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

proposalText2Proposal :: ProposalText -> Proposal
proposalText2Proposal ProposalText{..} =
    ( #description $ mkMTextUnsafe ptDescription
    , #newPolicy $ #urls urls)
  where
    urls = M.mapKeys (mkMTextUnsafe) . M.map decodeHU $ ptNewPolicy
    decodeHU (hash, url) = (encodeUtf8 hash, mkMTextUnsafe url)

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
