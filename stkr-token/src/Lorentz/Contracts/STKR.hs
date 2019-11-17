module Lorentz.Contracts.STKR
  ( Parameter (..)
  , Storage (..)

  , stkrContract
  ) where

import Lorentz
import Michelson.Typed.Haskell.Value (IsComparable)

type Hash = ByteString
type URL = MText

type NewCouncilParams =
  ( "epochId" :! Natural
  , "approvals" :! [(PublicKey, Signature)]
  , "newCouncil" :! [PublicKey]
  )
type AnnounceDecisionParams =
  ( "description" :! MText
  , "approvals" :! [(PublicKey, Signature)]
  , "newUrls" :! Map MText (Hash, URL)
  )

data Parameter
  = NewConcuil NewCouncilParams
  | AnnounceDecision AnnounceDecisionParams
  deriving stock Generic
  deriving anyclass IsoValue

data Storage = Storage
  { teamKeys :: [PublicKey] -- TODO: Why list here instead of Set?
  , councilKeys :: [PublicKey] -- TODO: Ahh, public keys are not comparible as I see
  , urls :: Map MText (Hash, URL)
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
  dip (do unionSets; size)
  stackType @(Natural : Natural : s)
  le

checkApprovedByMajority :: forall s. ([(PublicKey, Signature)] : ByteString : [PublicKey] : s) :-> s
checkApprovedByMajority = do
  isApprovedByMajority
  if_ (nop) (do push (); failCustom #majorityQuorumNotReached)

newCouncil :: Entrypoint NewCouncilParams Storage
newCouncil = do
  getField #approvals
  dip (do getField #newCouncil; pack)
  stackType @[ [(PublicKey, Signature)], ByteString, NewCouncilParams, Storage ]
  duupX @4; toField #teamKeys;
  dug @2
  stackType @[ [(PublicKey, Signature)], ByteString, [PublicKey], NewCouncilParams, Storage ]
  checkApprovedByMajority
  toField #newCouncil; setField #councilKeys
  nil; pair

announceDecision :: Entrypoint AnnounceDecisionParams Storage
announceDecision = do
  getField #approvals
  dip (do getField #newUrls; pack)
  duupX @4; toField #councilKeys
  dug @2
  checkApprovedByMajority
  toField #newUrls; setField #urls
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
