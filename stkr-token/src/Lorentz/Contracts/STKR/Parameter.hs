module Lorentz.Contracts.STKR.Parameter
  ( Parameter (..)
  , TransferParams
  , GetBalanceParams
  , Proposal
  , VoteForProposalParams
  , EnsureOwner(..)
  ) where

import Lorentz

import Lorentz.Contracts.STKR.Token.TypeDefs (TransferParams, GetBalanceParams)
import Lorentz.Contracts.STKR.Governance.TypeDefs (Proposal, VoteForProposalParams)
import Lorentz.Contracts.STKR.Misc.TypeDefs (EnsureOwner(..))

data Parameter
  = NewCouncil (EnsureOwner (Set KeyHash))
  | NewProposal (EnsureOwner Proposal)
  | VoteForProposal VoteForProposalParams
  | Transfer (EnsureOwner TransferParams)
  | GetBalance (View GetBalanceParams Natural)
  | GetTotalSupply (View () Natural)
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterEntryPoints Parameter where
  parameterEntryPoints = pepRecursive
