{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.STKR.Funding.Error
  (
  ) where

import Lorentz

type instance ErrorArg "invalidReceiver" = Address
instance (CustomErrorHasDoc "invalidReceiver") where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Could con make (contract unit) out of the receiver address"
  customErrArgumentSemantics =
    Just "Receiver address"
