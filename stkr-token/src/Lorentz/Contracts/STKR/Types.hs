{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lorentz.Contracts.STKR.Types
  ( Hash
  , URL
  , Rational
  ) where

import GHC.Real (Ratio(..), Rational)
import Lorentz (ByteString, MText, Generic, IsoValue)

type Hash = ByteString
type URL = MText

deriving instance Generic Rational
deriving instance IsoValue Rational
