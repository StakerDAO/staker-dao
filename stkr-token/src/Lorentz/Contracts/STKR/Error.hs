{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.STKR.Error
  () where

import Lorentz

type instance ErrorArg "senderCheckFailed" = Address
instance (CustomErrorHasDoc "senderCheckFailed") where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Access denied: transaction sender does not equal the expected one"
  customErrArgumentSemantics =
    Just "transaction sender does not equal the expected one"

type instance ErrorArg "notInCouncil" = KeyHash
instance (CustomErrorHasDoc "notInCouncil") where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Voting party is not a council member"
  customErrArgumentSemantics =
    Just "voting party is not a council member"

type instance ErrorArg "voteAlreadySubmitted" = "proposalId" :! Natural
instance (CustomErrorHasDoc "voteAlreadySubmitted") where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "This council member already voted in this epoch"
  customErrArgumentSemantics =
    Just "this council member already voted in this epoch"

type instance ErrorArg "invalidProposalId" = "proposalId" :! Natural
instance (CustomErrorHasDoc "invalidProposalId") where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "The supplied proposal id is invalid"
  customErrArgumentSemantics =
    Just "the supplied proposal id is invalid"

type instance ErrorArg "wrongStage" = "stageCounter" :! Natural
instance (CustomErrorHasDoc "wrongStage") where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Invalid stage for a request"
  customErrArgumentSemantics =
    Just "invalid stage for a request"

type instance ErrorArg "notEnoughFunds" = Integer
instance (CustomErrorHasDoc "notEnoughFunds") where
  customErrClass = ErrClassActionException
  customErrDocMdCause = "Not enough funds on from address"
  customErrArgumentSemantics =
    Just "not enough funds on from address"
