module Lorentz.Contracts.STKR.Parameter
  ( Parameter (..)
  , AnnounceDecisionParams
  ) where

import Lorentz

import Lorentz.Contracts.STKR.Storage (Hash, URL)

type AnnounceDecisionParams =
  ( "description" :! MText
  , "approvals" :! [(PublicKey, Signature)]
  , "newUrls" :! Map MText (Hash, URL)
  )

data Parameter
  = NewConcuil [PublicKey]
  | AnnounceDecision AnnounceDecisionParams
  | SetOperationsTeam Address
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterEntryPoints Parameter where
  parameterEntryPoints = pepPlain
