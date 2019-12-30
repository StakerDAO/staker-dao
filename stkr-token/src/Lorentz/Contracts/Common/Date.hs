module Lorentz.Contracts.Common.Date
  ( toOrdinalDate
  , extractDate
  , yearDayToMonthAndDay
  , isLeap

  , failIfNone
  -- TODO useful utility function, to be moved to other module
  ) where

import Lorentz

import Prelude ((*))
import Data.Time.Clock (UTCTime (..))
import Data.Time.Calendar (Day (..))
import Tezos.Core (timestampFromUTCTime)

extractDate
  :: Timestamp & s
  :-> "year" :! Integer & "month" :! Natural & "day" :! Natural & s
extractDate = do
  push $ timestampFromUTCTime (UTCTime (ModifiedJulianDay 0) 0)
  rsub
  let secondsInDay = 60 * 60 * 24
  dip (pushNat secondsInDay)
  ediv ; failIfNone [mt|extractDate: unexpected (1)|] ; car
  toNamed #mjd
  toOrdinalDate
  swap
  dup
  dip (isLeap # yearDayToMonthAndDay)

isLeap
  :: "year" :! Integer & s
  :-> "isLeap" :! Bool & s
isLeap = do
  fromNamed #year
  dup ; dip dup
  dip (pushNat 400)
  ediv ; failIfNone [mt|isLeap: unexpected (1)|] ; cdr
  int ; eq0
  swap
  dip (pushNat 100)
  ediv ; failIfNone [mt|isLeap: unexpected (1)|] ; cdr
  int ; neq0
  or
  swap
  dip (pushNat 4)
  ediv ; failIfNone [mt|isLeap: unexpected (1)|] ; cdr
  int ; eq0
  and
  toNamed #isLeap

yearDayToMonthAndDay
  :: "isLeap" :! Bool & "yearDay" :! Natural & s
  :-> "month" :! Natural & "day" :! Natural & s
yearDayToMonthAndDay = do
  push @[Natural] [31,30,31,30,31,31,30,31,30,31] ; swap
  fromNamed #isLeap
  if Holds
    then pushNat 29
    else pushNat 28
  cons ; pushNat 31 ; cons
  dip $ do
    fromNamed #yearDay
    pushNat 1 ; rsub -- -1 to year day to have it 0-based
    isNat
    failIfNone [mt|yearDayToMonthAndDay: year day <= 1|]
    some
    dip $ none # pushNat 0 # toNamed #month -- day, month
  iter $ do
    swap
    if IsSome
      then do
        stackType @(Natural {- year day remaining -}
                  & Natural {- amount of days in current month -}
                  & "month" :! Natural {- month counter -}
                  & Maybe ("day" :! Natural) {- day of month -} & _)
        dipN @2 (fromNamed #month # pushNat 1 # add # toNamed #month)
        dup
        dip swap
        sub
        isNat
        if IsSome
          then dip drop # some
          else do -- we reached the target month
            swap
            dip $ do
              dip drop
              pushNat 1 ; add -- +1 to day to have it 1-based
              toNamed #day
              some
            none
      else drop # none
  drop
  dip $
    failIfNone [mt|yearDayToMonthAndDay: year day is greater than number of days in year|]

failIfNone
  :: MText
  -> Maybe a & s :-> a & s
failIfNone s =
  if IsSome then nop else failUnexpected s

min3
  :: Natural & s :-> Natural & s
min3 = do
  dup
  push 3
  if IsGt then nop else drop # push 3

pushNat :: Natural -> s :-> Natural & s
pushNat = push

toOrdinalDate
  :: "mjd" :! Integer & s -- modified julian day
  :-> "yearDay" :! Natural & "year" :! Integer & s
toOrdinalDate = do
  let mjdZeroDay = 678575 -- number of days before 1858-11-17
  fromNamed #mjd ; pushNat mjdZeroDay; add
  let daysInQuadcent = 146097 -- days in 400 years
  dip (pushNat daysInQuadcent)
  ediv ; failIfNone [mt|toOrdinalDate: unexpected (1)|] ; unpair
  toNamed #quadCent
  dip $ do
    stackType @(Natural & _)
    dup
    let daysInCent = 36524 -- days in 100 years
    dip (pushNat daysInCent)
    ediv ; failIfNone [mt|toOrdinalDate: unexpected (2)|]; car
    min3
    dup
    dip (toNamed #cent # swap)
    pushNat daysInCent; mul
    rsub
    let daysIn4Years = 1461
    dip (pushNat daysIn4Years)
    ediv ; failIfNone [mt|toOrdinalDate: unexpected (3)|]; unpair
    toNamed #quad
    swap
    dup
    dip $ do
      let daysInYear = 365
      dip (pushNat daysInYear) ; ediv ; failIfNone [mt|toOrdinalDate: unexpected (4)|]; car
      min3
      dup
      dip (toNamed #y)
      pushNat daysInYear; mul
    sub
    pushNat 1; add
    isNat
    failIfNone [mt|toOrdinalDate: unexpected (5)|]
    toNamed #yearDay
  swap
  dip $ do
    fromNamed #quadCent
    pushNat 400
    mul
    pushNat 1; add -- +1
    dip (fromNamed #y) ; add -- +y
    dip (fromNamed #quad # pushNat 4 # mul) ; add -- +4*quad
    dip (fromNamed #cent # pushNat 100 # mul) ; add -- +100*cent
    toNamed #year
