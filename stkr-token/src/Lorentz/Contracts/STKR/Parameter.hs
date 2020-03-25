module Lorentz.Contracts.STKR.Parameter
  ( Parameter (..)
  , TransferParams
  , GetBalanceParams
  , Proposal
  , VoteForProposalParams
  , EnsureOwner(..)
  , OpsTeamEntrypointParam(..)
  , PermitOnFrozenParam(..)
  , PublicEntrypointParam(..)
  ) where

import Lorentz

import Lorentz.Contracts.STKR.Funding.TypeDefs (WithdrawParams)
import Lorentz.Contracts.STKR.Governance.TypeDefs
  (Proposal, VoteForProposalParams)
import Lorentz.Contracts.STKR.Misc.TypeDefs (EnsureOwner(..))
import Lorentz.Contracts.STKR.Token.TypeDefs (GetBalanceParams, TransferParams)

data OpsTeamEntrypointParam
  = NewCouncil (Set KeyHash)
  | Transfer TransferParams
  | NewProposal Proposal
  | Freeze ()
  deriving stock Generic
  deriving anyclass (IsoValue, HasTypeAnn)

data PublicEntrypointParam
  = VoteForProposal VoteForProposalParams
  | GetBalance (View GetBalanceParams Natural)
  | GetTotalSupply (View () Natural)
  | Fund ByteString
  deriving stock Generic
  deriving anyclass (IsoValue, HasTypeAnn)

data PermitOnFrozenParam
  = Withdraw WithdrawParams
  | SetSuccessor ("successor" :! (Lambda PublicEntrypointParam Operation))
  deriving stock Generic
  deriving anyclass (IsoValue, HasTypeAnn)

data Parameter
  = PublicEntrypoint PublicEntrypointParam
  | OpsTeamEntrypoint (EnsureOwner OpsTeamEntrypointParam)
  | PermitOnFrozen (EnsureOwner PermitOnFrozenParam)
  deriving stock Generic
  deriving anyclass (IsoValue, HasTypeAnn)

instance ParameterHasEntryPoints Parameter where
  type ParameterEntryPointsDerivation Parameter = EpdRecursive
