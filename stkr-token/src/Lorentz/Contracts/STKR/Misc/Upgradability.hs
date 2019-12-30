module Lorentz.Contracts.STKR.Misc.Upgradability
  ( ensureNotFrozen
  , freeze

  , forwardToSuccessor
  , setSuccessor
  , successorLambda
  ) where

import Lorentz

import Lorentz.Contracts.STKR.Misc.Error ()
import Lorentz.Contracts.STKR.Parameter (Parameter, PublicEntrypointParam)
import Lorentz.Contracts.STKR.Storage (Storage)

ensureNotFrozen :: Storage & s :-> s
ensureNotFrozen = do
  toField #frozen
  if Holds
  then do push (); failCustom #contractFrozen
  else nop

freeze :: Entrypoint () Storage
freeze = do
  drop
  push True
  setField #frozen
  nil; pair

forwardToSuccessor
  :: [PublicEntrypointParam, Lambda PublicEntrypointParam Operation, Storage]
  :-> ContractOut Storage
forwardToSuccessor = do
  exec # nil # swap # cons # pair

setSuccessor :: Entrypoint ("successor" :! (Lambda PublicEntrypointParam Operation)) Storage
setSuccessor = do
  duupX @2 @Storage
  toField #frozen
  if Holds
  then nop
  else do push (); failCustom #contractActive
  fromNamed #successor
  some
  setField #successor
  nil; pair

successorLambda
  :: ContractRef Parameter
  -> Lambda PublicEntrypointParam Operation
successorLambda newSTKR = do
  stackType @('[PublicEntrypointParam])
  wrap_ @Parameter #cPublicEntrypoint
  push (fromContractAddr newSTKR)
  contract @Parameter
  if IsSome
  then balance # dig @2 # transferTokens
  else (push (fromContractAddr @Parameter @Address newSTKR)) # failCustom #successorParameterCastError
