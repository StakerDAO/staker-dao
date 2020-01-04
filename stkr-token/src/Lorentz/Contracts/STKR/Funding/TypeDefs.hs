module Lorentz.Contracts.STKR.Funding.TypeDefs
  ( WithdrawParams
  ) where

import Lorentz

import Util.Named ((:!))

type WithdrawParams =
  ( "to" :! Address
  , "amount" :! Mutez
  )
