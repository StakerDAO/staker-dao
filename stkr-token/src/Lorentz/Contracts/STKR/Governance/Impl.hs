module Lorentz.Contracts.STKR.Governance.Impl
  ( newCouncil
  , newProposal
  , voteForProposal
  ) where

import Lorentz
import Lorentz.Contracts.Common (ensureSignatureValid, dupTop2)
import Lorentz.Contracts.STKR.Governance.Common
  (calcWinner, checkNotStages, checkPkCanVote, checkStage, splitCounter)
import Lorentz.Contracts.STKR.Governance.TypeDefs
  (Blake2BHash, CouncilDataToSign, Proposal, ProposalAndHash, ProposalId,
   VoteForProposalParams, blake2B_)
import Lorentz.Contracts.STKR.Parameter (Parameter)
import Lorentz.Contracts.STKR.Storage (Storage)

newCouncil :: GetCurrentStage -> Entrypoint (Set KeyHash) Storage
newCouncil curStage = do
  dip $ do
    updateStorage curStage
    dup # checkNotStages [3, 4]
  setField #councilKeys
  nil; pair

newProposal :: GetCurrentStage -> Entrypoint Proposal Storage
newProposal curStage = do
  dip $ do
    updateStorage curStage
    dup # checkStage 0
  dup
  pack
  blake2B_
  toNamed #proposalHash
  swap
  toNamed #proposal
  pair
  dip (getField #proposals)
  dupTop2; ensureNotSubmitted
  some
  dipN @2 $ do
    getField #lastProposalId
    push @Natural 1
    add
    dup
    dip $ setField #lastProposalId
  dig @2
  update
  stackType @'[Map ProposalId ProposalAndHash, Storage]
  setField #proposals
  nil; pair

  where
    ensureNotSubmitted :: ProposalAndHash & Map ProposalId ProposalAndHash & s :-> s
    ensureNotSubmitted = do
      toFieldNamed #proposalHash
      swap
      iter $ do
        cdr
        toField #proposalHash
        dip (dup # fromNamed #proposalHash)
        if IsEq
        then failCustom #duplicateProposal
        else nop
      drop @("proposalHash" :! Blake2BHash)



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
            emptyMap
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
  stackType @'[PublicKey, "proposalId" :! ProposalId, _, Signature, Storage]
  dug @2
  dup
  fromNamed #proposalId
  dip swap
  get

  if IsSome
    then do
      toField #proposalHash
      stackType @'[Blake2BHash, "proposalId" :! ProposalId, PublicKey, Signature, Storage]

      -- Check signature
      construct @CouncilDataToSign
        ( fieldCtor dup
          :& fieldCtor (self @Parameter # address)
          :& fieldCtor (dipN @4 (getField #stageCounter) # dig @4)
          :& RNil )
      dip drop
      stackType @'[CouncilDataToSign, "proposalId" :! ProposalId, PublicKey, Signature, Storage]
      pack
      dig @2
      dup
      dip $ do
        dip (dig @2)
        ensureSignatureValid

      -- Update storage
      stackType @'[PublicKey, "proposalId" :! ProposalId, Storage]
      hashKey
      dip some
      dipN @2 (getField #votes)
      update
      setField #votes
    else
      failCustom #invalidProposalId
  nil; pair
