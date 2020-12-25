{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.STKR.Misc.Error
  (
  ) where

import Lorentz

type instance ErrorArg "contractFrozen" = ()
instance (CustomErrorHasDoc "contractFrozen") where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Contract is frozen and migrating to newer version"
  customErrArgumentSemantics = Nothing

type instance ErrorArg "contractActive" = ()
instance (CustomErrorHasDoc "contractActive") where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Cannot set successor on active contract, freeze it first."
  customErrArgumentSemantics = Nothing

type instance ErrorArg "successorParameterCastError" = Address
instance (CustomErrorHasDoc "successorParameterCastError") where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Failed to cast successor parameter"
  customErrArgumentSemantics =
    Just "Successor address"
