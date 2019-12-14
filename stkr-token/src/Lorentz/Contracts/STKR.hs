module Lorentz.Contracts.STKR
  ( stkrContract
  , module Exports
  ) where

import Lorentz

import Lorentz.Contracts.STKR.Governance as Exports
import Lorentz.Contracts.STKR.Parameter as Exports
import Lorentz.Contracts.STKR.Storage as Exports
import qualified Lorentz.Contracts.STKR.Token as Token

stkrContract :: TimeConfig -> Contract Parameter Storage
stkrContract timeConfig = do
  unpair
  entryCase @Parameter (Proxy @PlainEntryPointsKind)
    ( #cNewCouncil /-> newCouncil (getCurrentStage timeConfig)
    , #cNewProposal /-> newProposal (getCurrentStage timeConfig)
    , #cVoteForProposal /-> voteForProposal (getCurrentStage timeConfig)
    , #cTransfer /-> Token.transfer
    , #cGetBalance /-> Token.getBalance
    , #cGetTotalSupply /-> Token.getTotalSupply
    )
