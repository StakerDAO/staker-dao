{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.STKR.Error
  () where

import Lorentz

type instance ErrorArg "majorityQuorumNotReached" = ()

instance (CustomErrorHasDoc "majorityQuorumNotReached") where
  customErrClass = ErrClassActionException

  customErrDocMdCause =
    "One or more supplied wallets are not whitelisted"

  customErrArgumentSemantics =
    Just "wallets found to be non whitelisted"


type instance ErrorArg "senderCheckFailed" = Address

instance (CustomErrorHasDoc "senderCheckFailed") where
  customErrClass = ErrClassActionException

  customErrDocMdCause =
    "Access denied: transaction sender does not equal the expected one"

  customErrArgumentSemantics =
    Just "transaction sender does not equal the expected one"


type instance ErrorArg "teamNotAssigned" = ()

instance (CustomErrorHasDoc "teamNotAssigned") where
  customErrClass = ErrClassActionException

  customErrDocMdCause =
    "Access denied: the owner has not yet assigned the Operations team"

  customErrArgumentSemantics =
    Just "operations team is not set"
