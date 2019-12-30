module Test.Lorentz.Contracts.TestCommon.Date
  ( spec_toOrdinalDate
  , spec_extractDate
  , spec_isLeap
  , spec_yearDayToMonthAndDay
  ) where

import Data.Functor.Identity (Identity(..))

import qualified Data.Time.Calendar as C
import qualified Data.Time.Calendar.OrdinalDate as C
import qualified Data.Time.Calendar.MonthDay as C
import qualified Data.Time.Clock as C
import qualified Data.Time.Clock.POSIX as C

import Test.Hspec (Spec, it)
import Test.HUnit ((@?=))
import qualified Test.Hspec.QuickCheck as HQ

import Tezos.Core (Timestamp (..), parseTimestamp)

import Lorentz (Rec (..))
import qualified Lorentz as L
import Lorentz.Test
import Named (arg)

import Lorentz.Contracts.Common.Date

spec_toOrdinalDate :: Spec
spec_toOrdinalDate = do
  test "1970-01-01" "2036-02-29"
  where
    test startS endS = it (startS <> ".." <> endS) $
        forM_ [start .. end] $ \dt_ ->
          run (C.toModifiedJulianDay dt_) @?=
            Right (second fromIntegral $ C.toOrdinalDate dt_)
      where
        start = parseDate startS
        end = parseDate endS
    parseDate = fromMaybe (error $ "spec_toOrdinalDate: unexpected "
                    <> "fail to parse date") . readMaybe
    run (dt_ :: Integer) = do
      let initStack = (Identity (#mjd dt_) :& RNil)
      resStack <- L.interpretLorentzInstr dummyContractEnv
        (toOrdinalDate L.# L.swap L.# L.dip (L.fromNamed #yearDay)
          L.# L.fromNamed #year L.# L.pair) initStack
      let Identity p :& RNil = resStack
      return p

spec_isLeap :: Spec
spec_isLeap = it "1900..2200" $ forM_ [1900..2200] $ \y ->
  run y @?= Right (C.isLeapYear y)
  where
    run y = do
      let initStack = (Identity (#year y) :& RNil)
      resStack <- L.interpretLorentzInstr dummyContractEnv isLeap initStack
      let Identity b :& RNil = resStack
      return (arg #isLeap b)

spec_yearDayToMonthAndDay :: Spec
spec_yearDayToMonthAndDay = do
  it "leap year -- all days" $ test True 366
  it "non-leap year -- all days" $ test False 365
  where
    test leap dayCount =
      forM_ [1..dayCount] $ \d ->
        run leap d @?= Right (C.dayOfYearToMonthAndDay leap $ fromIntegral d)
    run leap yd = do
      let initStack = (Identity (#isLeap leap) :& Identity (#yearDay yd) :& RNil)
      resStack <- L.interpretLorentzInstr dummyContractEnv yearDayToMonthAndDay initStack
      let Identity m :& Identity d :& RNil = resStack
      return (fromIntegral $ arg #month m, fromIntegral $ arg #day d)

spec_extractDate :: Spec
spec_extractDate = do
  it "manual" $ do
    run (parseTs "2019-10-05T10:00:00Z") @?= Right (2019, 10, 5)
    run (parseTs "2020-03-01T10:00:00Z") @?= Right (2020, 3, 1)
    run (parseTs "2024-02-29T00:00:00Z") @?= Right (2024, 2, 29)
    run (parseTs "2024-02-28T23:59:59Z") @?= Right (2024, 2, 28)
  HQ.prop "randomized" $ \ts ->
    run ts @?= Right (tsToDateTuple ts)
  where
    tsToDateTuple =
      C.toGregorian . C.utctDay .
      C.posixSecondsToUTCTime . unTimestamp
    parseTs = fromMaybe (error $ "spec_extractDate: unexpected "
                <> "fail to parse timestamp") . parseTimestamp
    run ts = do
      let initStack = (Identity ts :& RNil)
      resStack <- L.interpretLorentzInstr dummyContractEnv extractDate initStack
      let Identity y :& Identity m :& Identity d :& RNil = resStack
      return ( arg #year y
             , fromIntegral $ arg #month m :: Int
             , fromIntegral $ arg #day d :: Int)

