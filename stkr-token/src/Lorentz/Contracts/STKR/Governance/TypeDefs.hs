module Lorentz.Contracts.STKR.Governance.TypeDefs
  ( Hash
  , URL
  , Blake2BHash(..)
  , blake2B_
  , Policy
  , Proposal
  , ProposalAndHash
  , VoteForProposalParams
  , CouncilDataToSign(..)
  , TimeConfig(..)
  ) where

import Util.Named ((:!))
import Prelude (Show)

import Lorentz

type Hash = ByteString
type URL = MText

newtype Blake2BHash = Blake2BHash ByteString
  deriving stock Generic
  deriving newtype (IsoValue, IsoCValue, Show, Eq, CompareOpHs)

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

instance TypeHasDoc Blake2BHash where
  typeDocName _ = "Blake2BHash"
  typeDocMdDescription = "A typed wrapper over ByteString that represents a blake2b hash."
