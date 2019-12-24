module Lorentz.Contracts.STKR.Governance.Common
  ( TimeConfig(..)
  , getCurrentStage
  , checkPkCanVote
  , splitCounter
  , calcWinner
  , checkStage
  , checkNotStages
  ) where

import Util.Named ((:!))

import Lorentz
import Lorentz.Contracts.STKR.Governance.TypeDefs (TimeConfig(..), Policy)
import Lorentz.Contracts.STKR.Storage (Storage)
import Prelude (foldr)
import Lorentz.Contracts.STKR.Error ()
import Lorentz.Contracts.Common (countMapEls)

getCurrentStage
  :: TimeConfig -> s :-> (Natural & s)
getCurrentStage TestTC {..} = do
  push _stageDuration
  push _start
  now
  sub
  isNat
  if IsSome
    then nop
    else
      failUnexpected [mt|"getCurrentStage: now < start"|]
  ediv
  if IsSome
    then nop
    else
      failUnexpected [mt|"getCurrentStage: division by stageDuration produced an error"|]
  car
getCurrentStage _ = error "Not implemented yet"

checkPkCanVote
  :: KeyHash & Storage & s :-> s
checkPkCanVote = do
  dup
  dipN @2 (getField #councilKeys)
  dug @2
  mem
  stackType @(Bool & KeyHash & Storage & _)
  if Holds
    then do
      dip (toField #votes)
      get
      if IsSome
        then failCustom #voteAlreadySubmitted
        else nop
    else failCustom #notInCouncil

splitCounter
  :: Natural & s
      :-> "epoch" :! Natural & "stage" :! Natural & s
splitCounter = do
  push @Natural 4
  swap
  ediv
  if IsSome
    then do
      unpair
      toNamed #epoch
      dip (toNamed #stage)
    else
      failUnexpected [mt|"splitCounter: division by 4 produced an error"|]

calcWinner
  :: Storage & s :-> Maybe Policy & s
calcWinner = do
  getField #votes
  countMapEls (fromNamed #proposalId)
  none
  swap
  dig @2
  getField #councilKeys # size # toNamed #sz
  swap
  dip swap
  toField #proposals
  push @Natural 0
  swap
  iter $ do
    car # fromNamed #proposal
    dip $ do
      dup
      dip $ do
        dip $ dup # dug @3
        get
        if IsSome then nop else push 0
        push @Natural 2
        mul
        dip (dup # fromNamed #sz)
        compare # gt0
    dig @4
    dig @3
    if Holds
      then
        if IsSome
          then
            failUnexpected [mt|"calcWinner: more than one winner"|]
          else do
            cdr
            fromNamed #newPolicy
            some
      else dip drop
    dug @3
    dip swap
  drop # drop # drop

checkNotStages
  :: [Natural] -> Storage & s :-> s
checkNotStages is = foldr f nop is # drop
  where
    f i action = action # dup # checkStage' (push i # compare # neq0)

checkStage
  :: Natural -> Storage & s :-> s
checkStage i = checkStage' $ push i # compare # eq0

checkStage'
  :: (forall s' . Natural & s' :-> Bool & s')
  -> Storage & s :-> s
checkStage' predicate = do
  toField #stageCounter
  dup
  splitCounter # drop # fromNamed #stage
  predicate
  if Holds
    then drop
    else do
      toNamed #stageCounter
      failCustom #wrongStage