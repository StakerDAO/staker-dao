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
import Prelude (Show, Traversable(..), (<$>), maybe)
import Data.Map as M
import Text.Hex (decodeHex)
import Data.Aeson (FromJSON)

import Lorentz
import Michelson.Text (mkMText)

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
    description :: Text
  , newPolicy :: Map Text (Text, Text)
  } deriving (Generic, FromJSON)

proposalText2Proposal :: ProposalText -> Either Text Proposal
proposalText2Proposal ProposalText{..} = do
    decodedVals <- traverse decodeHU newPolicy
    decoded <- M.fromList <$> (mapM decodeUrl $ M.toList decodedVals)
    decodedDescription <- mkMText description
    pure
      ( #description $ decodedDescription
      , #newPolicy $ #urls decoded)
  where
    decodeUrl (turl, v) = do
      url <- mkMText turl
      return (url, v)
    decodeHU (thash, turl) = do
      hash <- decodeHex' thash
      url <- mkMText turl
      return (hash, url)
    decodeHex' txt = maybe (Left $ "Invalid hash: " <> txt) Right $ decodeHex txt


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
