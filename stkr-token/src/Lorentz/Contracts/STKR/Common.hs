module Lorentz.Contracts.STKR.Common
  ( ensureOwner
  ) where

import Lorentz

import Lorentz.Contracts.STKR.Error ()
import Lorentz.Contracts.STKR.Storage (Storage)


ensureOwner :: Storage ': s :-> s
ensureOwner = do
  toField #owner
  sender
  dip dup
  if IsEq
  then drop
  else failCustom #senderCheckFailed
