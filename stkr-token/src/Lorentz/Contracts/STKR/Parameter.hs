module Lorentz.Contracts.STKR.Parameter
  ( Parameter (..)
  , TransferParams
  , GetBalanceParams
  , Proposal
  , VoteForProposalParams
  , EnsureOwner(..)
  , OpsTeamEntrypointParam(..)
  , PublicEntrypointParam(..)
  ) where

import Lorentz

import Lorentz.Contracts.STKR.Funding.TypeDefs (WithdrawParams)
import Lorentz.Contracts.STKR.Token.TypeDefs (TransferParams, GetBalanceParams)
import Lorentz.Contracts.STKR.Governance.TypeDefs (Proposal, VoteForProposalParams)
import Lorentz.Contracts.STKR.Misc.TypeDefs (EnsureOwner(..))

data OpsTeamEntrypointParam
  = NewCouncil (Set KeyHash)
  | Transfer TransferParams
  | NewProposal Proposal
  | Freeze ()
  deriving stock Generic
  deriving anyclass IsoValue

data PublicEntrypointParam
  = VoteForProposal VoteForProposalParams
  | GetBalance (View GetBalanceParams Natural)
  | GetTotalSupply (View () Natural)
  | Fund ByteString
  deriving stock Generic
  deriving anyclass IsoValue

data Parameter
  = PublicEntrypoint PublicEntrypointParam
  | OpsTeamEntrypoint (EnsureOwner OpsTeamEntrypointParam)
  -- The following two methods may be called on frozen contract, so they're put separate
  | Withdraw (EnsureOwner WithdrawParams)
  | SetSuccessor (EnsureOwner ("successor" :! (Lambda PublicEntrypointParam Operation)))
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterEntryPoints Parameter where
  parameterEntryPoints = pepRecursive
