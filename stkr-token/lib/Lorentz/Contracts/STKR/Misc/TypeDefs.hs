module Lorentz.Contracts.STKR.Misc.TypeDefs
  ( EnsureOwner(..)
  ) where

import Prelude

import Lorentz

newtype EnsureOwner a = EnsureOwner { unEnsureOwner :: a }
  deriving stock Generic
  deriving anyclass (IsoValue, HasTypeAnn)

-- TODO: Resulting doc render not tested. Take care of this when autodoc is ready.
instance (TypeHasDoc a) => TypeHasDoc (EnsureOwner a) where
  typeDocMdDescription = "This type reprsents requirires that this entrypoint caller must be owner."
  typeDocMdReference = poly1TypeDocMdReference
  typeDocDependencies p = genericTypeDocDependencies p <> [SomeTypeWithDoc (Proxy @a)]
  typeDocHaskellRep proxy = typeDocHaskellRep (unEnsureOwner <$> proxy)
  typeDocMichelsonRep proxy = typeDocMichelsonRep (unEnsureOwner <$> proxy)
