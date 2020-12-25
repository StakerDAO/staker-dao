module Lorentz.Contracts.STKR.Token.TypeDefs
  ( TransferParams
  , GetBalanceParams
  ) where

import Lorentz
import Util.Named ((:!))

type TransferParams = ("from" :! Address, "to" :! Address, "value" :! Natural)
type GetBalanceParams = ("owner" :! Address)
