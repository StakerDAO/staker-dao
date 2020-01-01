{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.STKR.Governance.Aeson
  ( JPolicy
  , JProposal
  , fromJProposal
  ) where

import Prelude
import Data.Aeson (FromJSON, FromJSONKey (..), FromJSONKeyFunction (..))

import Michelson.Text (mkMText)

import Lorentz (MText, IsoValue)

import Lorentz.Contracts.STKR.Governance.TypeDefs

data JPolicy = JPolicy
  { urls :: Map MText (Sha256Hash, URL)
  } deriving (IsoValue, Generic, FromJSON)

data JProposal = JProposal
  { description :: MText
  , newPolicy :: JPolicy
  } deriving (IsoValue, Generic, FromJSON)

instance FromJSONKey MText where
  fromJSONKey = FromJSONKeyTextParser $
    either (fail . toString) pure . mkMText

fromJProposal :: JProposal -> Proposal
fromJProposal JProposal {..} =
  (#description description, #newPolicy policy_)
  where
    policy_ = #urls (urls newPolicy)
