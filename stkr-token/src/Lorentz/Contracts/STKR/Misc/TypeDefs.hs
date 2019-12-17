module Lorentz.Contracts.STKR.Misc.TypeDefs
  ( EnsureOwner(..)
  ) where

import Lorentz

newtype EnsureOwner a = EnsureOwner a
  deriving stock Generic
  deriving anyclass IsoValue

instance (TypeHasDoc a) => TypeHasDoc (EnsureOwner a) where
  typeDocMdDescription = ""
  typeDocMdReference = undefined
  typeDocDependencies = undefined
  typeDocHaskellRep = undefined
  typeDocMichelsonRep = undefined
