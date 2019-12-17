module Lorentz.Contracts.STKR.Misc.Impl
  ( ensureOwnerI
  , ensureOwner
  ) where

import Lorentz
import Lorentz.Coercions (coerce_)

import Lorentz.Contracts.STKR.Error ()
import Lorentz.Contracts.STKR.Misc.TypeDefs
import Lorentz.Contracts.STKR.Storage (Storage)

ensureOwnerI :: forall a s. EnsureOwner a & Storage & s :-> a & s
ensureOwnerI= do
  dip $ do
    toField #owner
    sender
    dip dup
    if IsEq
    then drop
    else failCustom #senderCheckFailed
  coerce_

ensureOwner
  :: forall a si sr.
  a & Storage & si :-> sr
  -> EnsureOwner a & Storage & si :-> sr
ensureOwner instr = do
  duupX @2
  swap
  ensureOwnerI
  instr
