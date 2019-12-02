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
