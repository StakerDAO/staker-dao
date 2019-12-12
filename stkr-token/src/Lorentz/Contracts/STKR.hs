module Lorentz.Contracts.STKR
  ( Parameter (..)
  , Storage (..)
  , TimeConfig (..)
  , CouncilDataToSign (..)
  , Blake2BHash (..)
  , stkrContract
  , Proposal
  , Policy
  , VoteForProposalParams
  , URL
  , Hash
  ) where

import Lorentz

import Lorentz.Contracts.STKR.Common (TimeConfig (..), ensureOwner,
                                      getCurrentStage, checkPkCanVote, splitCounter,
                                      calcWinner, checkNotStages, checkStage)
import Lorentz.Contracts.STKR.Error ()
import Lorentz.Contracts.STKR.Parameter (VoteForProposalParams, Parameter(..))
import Lorentz.Contracts.STKR.Storage (Blake2BHash (..), Hash, Policy, Proposal,
                                      Storage (..), URL, blake2B_)
import Lorentz.Contracts.Common (listAt, ensureSignatureValid)

data CouncilDataToSign = CouncilDataToSign
  { cdProposalHash :: Blake2BHash
  , cdContractAddr :: Address
  , cdStageCounter :: Natural
  }
  deriving stock Generic
  deriving anyclass IsoValue

stkrContract :: TimeConfig -> Contract Parameter Storage
stkrContract timeConfig = do
  unpair
  entryCase @Parameter (Proxy @PlainEntryPointsKind)
    ( #cNewCouncil /-> newCouncil (getCurrentStage timeConfig)
    , #cNewProposal /-> newProposal (getCurrentStage timeConfig)
    , #cVoteForProposal /-> voteForProposal (getCurrentStage timeConfig)
    )

newCouncil :: GetCurrentStage -> Entrypoint (Set KeyHash) Storage
newCouncil curStage = do
  dip $ do
    updateStorage curStage
    dup # checkNotStages [3, 4]
    dup # ensureOwner
  setField #councilKeys
  nil; pair

newProposal :: GetCurrentStage -> Entrypoint Proposal Storage
newProposal curStage = do
  dip $ do
    updateStorage curStage
    dup # checkStage 0
    dup # ensureOwner
  dup
  pack
  blake2B_
  toNamed #proposalHash
  swap
  toNamed #proposal
  pair
  dip (getField #proposals)
  cons
  setField #proposals
  nil; pair

type GetCurrentStage = forall s. s :-> Natural & s

-- | Checks current time and updates state of
-- contract according to it
updateStorage
  :: GetCurrentStage -> Storage & s :-> Storage & s
updateStorage curStage = do
  getFieldNamed #stageCounter
  dup
  curStage
  dup; dip (swap # fromNamed #stageCounter)
  compare
  dup
  if IsZero
    then drop # drop # drop
    else do
      lt0
      if Holds
        then drop # failCustom #wrongStage
        -- current stage less than counter
        else impl
  where
    impl
      :: Natural & ("stageCounter" :! Natural) & Storage & s
      :-> Storage & s
    impl = do
      dup
      dip $ do
        dip (fromNamed #stageCounter # splitCounter # dip drop # fromNamed #epoch)
        splitCounter # dip drop # fromNamed #epoch
        if IsEq
          then nop
          else do
            dup
            calcWinner
            if IsSome
              then setField #policy
              else nop
            nil
            setField #proposals
            emptyMap
            setField #votes
      setField #stageCounter

voteForProposal :: GetCurrentStage -> Entrypoint VoteForProposalParams Storage
voteForProposal curStage = do
  dip (updateStorage curStage)

  -- Check that it's third stage (Voting)
  dip (dup # checkStage 2)

  -- Check that public key is eligible to vote
  dip dup
  getField #votePk
  dup
  hashKey
  dip (dig @2)
  checkPkCanVote

  -- Extract fields from params
  dip $ do
    dip dup
    dup
    toFieldNamed #proposalId
    dip (toField #voteSig # swap # toField #proposals)

  -- Get proposal from storage
  stackType @'[PublicKey, "proposalId" :! Natural, _, Signature, Storage]
  dug @2
  dup
  fromNamed #proposalId
  dig @2
  listAt

  if IsSome
    then do
      toField #proposalHash
      stackType @'[Blake2BHash, "proposalId" :! Natural, PublicKey, Signature, Storage]

      -- Check signature
      construct @CouncilDataToSign
        ( fieldCtor dup
          :& fieldCtor (self @Parameter # address)
          :& fieldCtor (dipN @4 (getField #stageCounter) # dig @4)
          :& RNil )
      dip drop
      stackType @'[CouncilDataToSign, "proposalId" :! Natural, PublicKey, Signature, Storage]
      pack
      dig @2
      dup
      dip $ do
        dip (dig @2)
        ensureSignatureValid

      -- Update storage
      stackType @'[PublicKey, "proposalId" :! Natural, Storage]
      hashKey
      dip some
      dipN @2 (getField #votes)
      update
      setField #votes
    else
      failCustom #invalidProposalId
  nil; pair
