{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.Multisig.Error
  () where

import Lorentz

type instance ErrorArg "invalidNonce" = ()
instance (CustomErrorHasDoc "invalidNonce") where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "The supplied nonce is invalid"

type instance ErrorArg "invalidStakerContract" = ()
instance (CustomErrorHasDoc "invalidStakerContract") where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "The supplied contract type does not match the parameter type"

type instance ErrorArg "nonzeroAmountReceived" = ()
instance (CustomErrorHasDoc "nonzeroAmountReceived") where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Non-zero amount of Tz was sent to the multisig contract"
