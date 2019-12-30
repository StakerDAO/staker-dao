module Lorentz.Contracts.STKR.Misc.EnsureOwner
  ( ensureOwner
  , withOwnerEnsured
  ) where

import Lorentz
import Lorentz.Coercions (coerce_)

import Lorentz.Contracts.STKR.Error ()
import Lorentz.Contracts.STKR.Misc.TypeDefs
import Lorentz.Contracts.STKR.Storage (Storage)

ensureOwner :: forall a s. EnsureOwner a & Storage & s :-> a & s
ensureOwner= do
  dip $ do
    toField #owner
    sender
    dip dup
    if IsEq
    then drop
    else failCustom #senderCheckFailed
  coerce_

withOwnerEnsured
  :: forall a si sr.
  a & Storage & si :-> sr
  -> EnsureOwner a & Storage & si :-> sr
withOwnerEnsured instr = do
  duupX @2
  swap
  ensureOwner
  instr
