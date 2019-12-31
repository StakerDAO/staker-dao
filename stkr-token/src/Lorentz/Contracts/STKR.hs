module Lorentz.Contracts.STKR
  ( stkrContract
  , module Exports
  , successorLambda
  ) where

import Lorentz

import Lorentz.Contracts.STKR.Governance as Exports
import Lorentz.Contracts.STKR.Misc.EnsureOwner (withOwnerEnsured)
import Lorentz.Contracts.STKR.Misc.Upgradability
  (ensureNotFrozen, forwardToSuccessor, freeze, setSuccessor, successorLambda)
import Lorentz.Contracts.STKR.Parameter as Exports
import Lorentz.Contracts.STKR.Storage as Exports
import Lorentz.Contracts.STKR.Funding as Exports
import qualified Lorentz.Contracts.STKR.Token as Token


publicRouter
  :: TimeConfig
  -> [PublicEntrypointParam, Storage] :-> ContractOut Storage
publicRouter timeConfig = caseT @PublicEntrypointParam $
  ( #cVoteForProposal /-> voteForProposal (getCurrentStage timeConfig)
  , #cGetBalance /-> Token.getBalance
  , #cGetTotalSupply /-> Token.getTotalSupply
  , #cFund /-> fund
  )

opsRouter
  :: TimeConfig
  -> [OpsTeamEntrypointParam, Storage] :-> ContractOut Storage
opsRouter timeConfig = caseT @OpsTeamEntrypointParam $
  ( #cNewCouncil /-> newCouncil (getCurrentStage timeConfig)
  , #cTransfer /-> Token.transfer
  , #cNewProposal /-> newProposal (getCurrentStage timeConfig)
  , #cFreeze /-> freeze
  )

frozenRouter :: [PermitOnFrozenParam, Storage] :-> ContractOut Storage
frozenRouter = caseT @PermitOnFrozenParam $
  ( #cWithdraw /-> withdraw
  , #cSetSuccessor /-> setSuccessor
  )

entrypointRouter
  :: TimeConfig
  -> [Parameter, Storage] :-> ContractOut Storage
entrypointRouter timeConfig = do
  -- TODO: replace caseT to entryCase to generate proper docs
  caseT @Parameter
    ( #cPublicEntrypoint /-> do
        duupX @2 @Storage; toField #successor
        if IsSome
        then swap # forwardToSuccessor
        else do
          duupX @2 @Storage; ensureNotFrozen
          publicRouter timeConfig
    , #cOpsTeamEntrypoint /-> do
        duupX @2 @Storage; ensureNotFrozen
        withOwnerEnsured $ opsRouter timeConfig
    , #cPermitOnFrozen /-> withOwnerEnsured frozenRouter
    )

stkrContract :: TimeConfig -> Contract Parameter Storage
stkrContract timeConfig = unpair # entrypointRouter timeConfig
