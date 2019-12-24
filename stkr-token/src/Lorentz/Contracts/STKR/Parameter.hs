module Lorentz.Contracts.STKR.Parameter
  ( Parameter (..)
  ) where

import Lorentz

import Lorentz.Contracts.STKR.Token.TypeDefs (TransferParams, GetBalanceParams)
import Lorentz.Contracts.STKR.Governance.TypeDefs (Proposal, VoteForProposalParams)

data Parameter
  = NewCouncil (Set KeyHash)
  | NewProposal Proposal
  | VoteForProposal VoteForProposalParams
  | Transfer TransferParams
  | GetBalance (View GetBalanceParams Natural)
  | GetTotalSupply (View () Natural)
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterEntryPoints Parameter where
  parameterEntryPoints = pepPlain
