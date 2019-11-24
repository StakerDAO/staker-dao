module Lorentz.Contracts.STKR
  ( Parameter (..)
  , Storage (..)

  , stkrContract
  ) where

import Lorentz

import Lorentz.Contracts.STKR.Common (checkApprovedByMajority, ensureOwner, ensureTeam)
import Lorentz.Contracts.STKR.Error ()
import Lorentz.Contracts.STKR.Parameter (AnnounceDecisionParams, Parameter(..))
import Lorentz.Contracts.STKR.Storage (Storage(..))

stkrContract :: Contract Parameter Storage
stkrContract = do
  unpair
  entryCase @Parameter (Proxy @PlainEntryPointsKind)
    ( #cNewConcuil /-> newCouncil
    , #cAnnounceDecision /-> announceDecision
    , #cSetOperationsTeam /-> setOperationsTeam
    )

newCouncil :: Entrypoint [PublicKey] Storage
newCouncil = do
  dip (dup # ensureTeam)
  setField #councilKeys
  nil; pair

announceDecision :: Entrypoint AnnounceDecisionParams Storage
announceDecision = do
  getField #approvals
  dip (do getField #newUrls; pack)
  duupX @4; toField #councilKeys
  dug @2
  checkApprovedByMajority
  toField #newUrls; setField #urls
  nil; pair

setOperationsTeam :: Entrypoint Address Storage
setOperationsTeam = do
  dip (dup # ensureOwner)
  some
  setField #team
  nil; pair
