module Lorentz.Contracts.STKR.Common
  ( intersectSets

  , isApprovedByMajority
  , checkApprovedByMajority

  , ensureOwner
  ) where

import Lorentz
import Michelson.Typed.Haskell.Value (IsComparable)

import Lorentz.Contracts.Common (listToSet)
import Lorentz.Contracts.STKR.Error ()
import Lorentz.Contracts.STKR.Storage (Storage)

intersectSets :: forall a s. (IsComparable a, KnownCValue a) => (Set a : Set a : s) :-> ((Set a) : s)
intersectSets = do
  dip (toNamed #y)
  toNamed #x
  emptySet @a; toNamed #result
  stackType @(("result" :! Set a) : ("x" :! Set a) : ("y" :! Set a) : s)
  swap
  stackType @(("x" :! Set a) : ("result" :! Set a) : ("y" :! Set a) : s)
  fromNamed #x
  iter (do
          stackType @(a : ("result" :! Set a) : ("y" :! Set a) : s);
          duupX @3
          stackType @(("y" :! Set a) : a : ( "result" :! Set a) : ("y" :! Set a) : s);
          fromNamed #y
          duupX @2
          stackType @(a : (Set a) : a : ("result" :! Set a) : ("y" :! Set a) : s);
          mem
          stackType @(Bool : a : ("result" :! Set a) : ("y" :! Set a) : s);
          if_ (do swap; fromNamed #result; swap; push True; swap; update; toNamed #result) (drop))
  stackType @(("result" :! Set a) : ("y" :! Set a) : s)
  fromNamed #result
  dropX @1

isApprovedByMajority :: forall s. ([(PublicKey, Signature)] : ByteString : [PublicKey] : s) :-> (Bool : s)
isApprovedByMajority = do
  map (do
          stackType @((PublicKey, Signature) : ByteString : [PublicKey] : s)
          unpair; duupX @3; dig @2; duupX @3
          stackType @(PublicKey : Signature : ByteString : PublicKey : ByteString : [PublicKey] : s)
          checkSignature
          if_
            hashKey
            (failCustom #invalidSignature)
      )
  listToSet
  dropX @1
  dig @1; map hashKey; listToSet;
  dup; size; push (2 :: Natural); swap; ediv; ifSome (do car; push (1 :: Natural); add) (fail_) -- TODO: impossible exception
  dip (do intersectSets; size)
  stackType @(Natural : Natural : s)
  le

checkApprovedByMajority :: forall s. ([(PublicKey, Signature)] : ByteString : [PublicKey] : s) :-> s
checkApprovedByMajority = do
  isApprovedByMajority
  if_ (nop) (do push (); failCustom #majorityQuorumNotReached)

ensureOwner :: Storage ': s :-> s
ensureOwner = do
  toField #owner
  sender
  dip dup
  if IsEq
  then drop
  else failCustom #senderCheckFailed
