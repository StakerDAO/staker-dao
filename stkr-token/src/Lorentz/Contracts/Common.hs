{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.Common
  ( dupTop2
  , listToSet
  ) where

import Lorentz
import Michelson.Typed.Haskell.Value (IsComparable)

-- | Duplicates first two stack entries.
dupTop2 :: a ': b ': s :-> a ': b ': a ': b ': s
dupTop2 = do
  duupX @2; duupX @2

-- | Converts a list of comparable values to a set,
-- removes duplicates as a consequence.
listToSet
  :: forall a s. (IsComparable a, KnownCValue a)
  => ((List a) : s) :-> ((Set a) : s)
listToSet = do
  dip (emptySet @a);
  iter (do dip (push True); update)

type instance ErrorArg "invalidSignature" = PublicKey

instance (CustomErrorHasDoc "invalidSignature") where
  customErrClass = ErrClassActionException

  customErrDocMdCause =
    "At least one of the supplied signatures was invalid"

  customErrArgumentSemantics =
    Just "one of the signatures was found to be invalid"

type instance ErrorArg "majorityQuorumNotReached" = ()

instance (CustomErrorHasDoc "majorityQuorumNotReached") where
  customErrClass = ErrClassActionException

  customErrDocMdCause =
    "The number of signatures provided was less than the minimal one"

  customErrArgumentSemantics =
    Just "the number of signatures is too small"
