{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.Common
  ( dupTop2
  , listToSet
  , listAt
  , ensureSignatureValid
  , countMapEls
  ) where

import Michelson.Typed (IterOpEl, T (Tc))
import Data.Typeable (Typeable)
import Data.Singletons (SingI)
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

ensureSignatureValid
  :: PublicKey & Signature & ByteString & s1 :-> s1
ensureSignatureValid = do
  dup
  dip checkSignature
  swap
  if Holds
  then drop
  else failCustom #invalidSignature

listAt
  ::
    ( IterOpHs c
    , IsoValue (IterOpElHs c)
    , Typeable (IterOpEl (ToT c))
    , SingI (IterOpEl (ToT c))
    )
  => c & Natural & s :-> Maybe (IterOpElHs c) & s
listAt = do
  dip (int # none)
  iter $ do
    dip swap
    swap
    dup
    if IsZero
      then
        dipN @2 drop # dip some
      else
        swap # drop
    push @Integer 1
    rsub
    swap
  dip drop

countMapEls
  ::
    ( ToT c ~ 'Tc (ToCT c)
    , ToT a ~ 'Tc (ToCT a)
    , IsoValue c
    , Typeable (ToCT c)
    , SingI (ToCT c)
    )
  => (forall s'. b & s' :-> c & s')
  -> Map a b & s :-> Map c Natural & s
countMapEls valToKey = do
  emptyMap
  swap
  iter $ do
    dip dup
    cdr
    valToKey
    dup
    dip swap
    get
    if IsSome
      then do
        push @Natural 1
        add
        some
        swap
        update
      else do
        dip $ push @(Maybe Natural) (Just 1)
        update

