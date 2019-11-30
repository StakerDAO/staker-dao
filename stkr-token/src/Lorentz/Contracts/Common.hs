{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.Common
  () where

import Lorentz

type instance ErrorArg "invalidSignature" = PublicKey

instance (CustomErrorHasDoc "invalidSignature") where
  customErrClass = ErrClassActionException

  customErrDocMdCause =
    "At least one of the supplied signatures was invalid"

  customErrArgumentSemantics =
    Just "one of the signatures was found to be invalid"

type instance ErrorArg "quorumNotReached" = ()

instance (CustomErrorHasDoc "quorumNotReached") where
  customErrClass = ErrClassActionException

  customErrDocMdCause =
    "The number of signatures provided was less than the minimal one"

  customErrArgumentSemantics =
    Just "the number of signatures is too small"
