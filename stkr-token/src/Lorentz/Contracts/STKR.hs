module Lorentz.Contracts.STKR () where

import Lorentz

type NewCouncilParams = ("epochId" :! Natural, "approvals" :! [(PublicKey, Signature)], "newCouncil" :! [PublicKey])

data Parameter
  = NewConcuil NewCouncilParams
  | AnnounceDecision (ByteString)
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
    , #cAnnounceDecision /-> undefined
    )

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
            fail_ -- TODO: rewrite to `failWith`
      )
  stackType @[ [KeyHash], ByteString, NewCouncilParams, Storage ]
  drop; drop;
  toField #newCouncil; setField #councilKeys
  nil; pair
