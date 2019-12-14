module Lorentz.Contracts.STKR.Parameter
  ( Parameter (..)
  , VoteForProposalParams
  ) where

import Lorentz

import Lorentz.Contracts.STKR.Storage (Proposal)
import Lorentz.Contracts.STKR.Token (TransferParams, GetBalanceParams)

type VoteForProposalParams =
  ( "proposalId" :! Natural
  , "votePk" :! PublicKey
  , "voteSig" :! Signature
  )

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
