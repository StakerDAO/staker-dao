module Lorentz.Contracts.STKR.Funding.Impl
  ( fund
  , withdraw
  ) where

import Lorentz

import Lorentz.Contracts.STKR.Funding.Error ()
import Lorentz.Contracts.STKR.Funding.TypeDefs (WithdrawParams)
import Lorentz.Contracts.STKR.Storage (Storage)

fund :: Entrypoint ByteString Storage
fund = do
  drop; nil; pair

withdraw :: Entrypoint WithdrawParams Storage
withdraw = do
  getField #to
  dup
  contract @()
  if IsSome
  then dip drop
  else failCustom #invalidReceiver
  swap
  toField #amount
  unit
  transferTokens
  dip nil; cons
  pair
