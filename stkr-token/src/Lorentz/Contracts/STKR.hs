module Lorentz.Contracts.STKR
  ( Parameter (..)
  , Storage (..)

  , stkrContract
  ) where

import Lorentz
import Michelson.Typed.Haskell.Value (IsComparable)

type NewCouncilParams = ("epochId" :! Natural, "approvals" :! [(PublicKey, Signature)], "newCouncil" :! [PublicKey])
type AnnounceDecisionParams = ByteString

data Parameter
  = NewConcuil NewCouncilParams
  | AnnounceDecision AnnounceDecisionParams
  deriving stock Generic
  deriving anyclass IsoValue

type Hash = ByteString
type URL = ByteString

data Storage = Storage
  { teamKeys :: [PublicKey] -- TODO: Why list here instead of Set?
  , councilKeys :: [PublicKey] -- TODO: Ahh, public keys are not comparible as I see
  , urls :: Map ByteString (Hash, URL)
  }
  deriving stock Generic
  deriving anyclass IsoValue


stkrContract :: Contract Parameter Storage
stkrContract = do
  unpair
  entryCase @Parameter (Proxy @PlainEntryPointsKind)
    ( #cNewConcuil /-> newCouncil
    , #cAnnounceDecision /-> announceDecision
    )

type instance ErrorArg "invalidSignature" = PublicKey

instance (CustomErrorHasDoc "invalidSignature") where
  customErrClass = ErrClassActionException

  customErrDocMdCause =
    "One or more supplied wallets are not whitelisted"

  customErrArgumentSemantics =
    Just "wallets found to be non whitelisted"


type instance ErrorArg "majorityQuorumNotReached" = ()

instance (CustomErrorHasDoc "majorityQuorumNotReached") where
  customErrClass = ErrClassActionException

  customErrDocMdCause =
    "One or more supplied wallets are not whitelisted"

  customErrArgumentSemantics =
    Just "wallets found to be non whitelisted"


newCouncil :: Entrypoint NewCouncilParams Storage
newCouncil = do
  stackType @[NewCouncilParams, Storage]
  getField #approvals
  stackType @[ [(PublicKey, Signature)], NewCouncilParams, Storage ]
  dip (do getField #newCouncil; pack)
  stackType @[ [(PublicKey, Signature)], ByteString, NewCouncilParams, Storage ]
  map (do
          stackType @[ (PublicKey, Signature), ByteString, NewCouncilParams, Storage ]
          unpair; duupX @3; dig @2; duupX @3
          stackType @[ PublicKey, Signature, ByteString, PublicKey, ByteString, NewCouncilParams, Storage ]
          checkSignature
          if_
            hashKey
            (failCustom #invalidSignature)
      )
  listToSet
  stackType @[ Set KeyHash, ByteString, NewCouncilParams, Storage ]
  duupX @4; toField #teamKeys;
  map hashKey; listToSet;
  dup; size; push (2 :: Natural); swap; ediv; ifSome (do car; push (1 :: Natural); add) (fail_) -- TODO: impossible exception
  stackType @[ Natural, Set KeyHash, Set KeyHash, ByteString, NewCouncilParams, Storage ]
  dip (do unionSets; size)
  ifLe (nop) (do push (); failCustom #majorityQuorumNotReached)
  drop;
  toField #newCouncil; setField #councilKeys
  nil; pair


listToSet :: forall a s. (IsComparable a, KnownCValue a) => ((List a) : s) :-> ((Set a) : s)
listToSet = do
  dip (emptySet @a);
  iter (do dip (push True); update)

unionSets :: forall a s. (IsComparable a, KnownCValue a) => (Set a : Set a : s) :-> ((Set a) : s)
unionSets = do
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

announceDecision :: Entrypoint AnnounceDecisionParams Storage
announceDecision = do drop; nil; pair
