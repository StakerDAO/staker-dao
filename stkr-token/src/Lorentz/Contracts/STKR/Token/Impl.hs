module Lorentz.Contracts.STKR.Token.Impl
  ( transfer
  , getBalance
  , getTotalSupply
  ) where

import Lorentz

import Lorentz.Contracts.STKR.Error ()
import Lorentz.Contracts.STKR.Misc (EnsureOwner, ensureOwner)
import Lorentz.Contracts.STKR.Storage (Storage)
import Lorentz.Contracts.STKR.Token.TypeDefs (TransferParams, GetBalanceParams)


transfer :: Entrypoint (EnsureOwner TransferParams) Storage
transfer = ensureOwner $ do
  swap
  duupX @2; getField #value; swap; toField #from
  debitFrom
  swap; getField #value; swap; toField #to
  creditTo
  nil; pair

getBalance :: Entrypoint (View GetBalanceParams Natural) Storage
getBalance = view_ $ do
  unpair
  fromNamed #owner;
  dip $ toField #ledger
  get
  if IsNone
  then push 0
  else nop

getTotalSupply :: Entrypoint (View () Natural) Storage
getTotalSupply = view_ $ do
  unpair; drop
  toField #totalSupply

creditTo
  :: forall s. Address & Natural & Storage & s :-> Storage & s
creditTo = do
  duupX @3
  toField #ledger; dup
  duupX @3; get
  if IsNone
  then push 0
  else nop
  dig @3
  add
  some
  dig @2
  update
  setField #ledger

debitFrom
  :: forall s. Address & Natural & Storage & s :-> Storage & s
debitFrom = do
  duupX @3; toField #ledger; dup;
  duupX @3; get; assertSome [mt|Address to be debited is not in ledger|]
  dig @3; swap; subGt0
  dig @2
  update
  setField #ledger

subGt0 :: Natural ': Natural ': s :-> Maybe Natural ': s
subGt0 = do
  sub
  dup
  isNat
  if IsSome
  then do
    swap; eq0
    if Holds
      then drop # none
      else some
  else failCustom #notEnoughFunds
