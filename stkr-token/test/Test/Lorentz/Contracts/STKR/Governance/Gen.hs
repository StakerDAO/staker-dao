module Test.Lorentz.Contracts.STKR.Governance.Gen
  ( EpochDesc
  , num
  , stages
  , votes

  , EpochVotes
  , distribution
  , winner


  , genEpochDesc
  , genProposal
  , genProposals
  ) where

import Data.Time.Calendar (addDays, addGregorianMonthsClip, fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Lens.Micro (ix)
import Lens.Micro.TH (makeLenses)
import System.Random (Random)
import Tezos.Core (Timestamp, timestampFromUTCTime)
import Tezos.Crypto (SecretKey)

import Test.QuickCheck (Gen, arbitrary, choose, vectorOf)

import qualified Lorentz.Contracts.STKR as STKR

import Test.Lorentz.Contracts.STKR.Common ()

govEpochsStartingAt :: Integer -> Int -> [(Natural, [UTCTime])]
govEpochsStartingAt year month =
  startDayToEpoch <$> zip [0..] epochStartDays
  where
    epochStartDays = iterate (addGregorianMonthsClip 1) $
      fromGregorian year month 1
    dayToUTCTimeAtMidnight day = UTCTime day (secondsToDiffTime 0)
    startDayToEpoch (epochNum, startDay) =
      (epochNum,) $
        dayToUTCTimeAtMidnight <$>
          (flip addDays) startDay <$>
          [0, 7, 14, 21]


genProposal :: Gen STKR.Proposal
genProposal = arbitrary

genProposals :: (Int, Int) -> Gen [STKR.Proposal]
genProposals len = choose len >>= flip vectorOf genProposal

votesDistribution
  :: Int
  -> Int
  -> Gen ([Int], Int)
votesDistribution votesAvailible candidates = do
  winner <- choose (0, candidates - 1)
  winnerGap <- choose (1, votesAvailible - votesAvailible `div` 2)
  let winnerVotes = votesAvailible `div` 2 + winnerGap
  let votesLeft = votesAvailible - winnerVotes
  votesWithoutWinner <- splitNum votesLeft (candidates - 1)
  let votesWithWinner = insertAt winner winnerVotes votesWithoutWinner
  pure (votesWithWinner, winner)
  where
    splitNum :: (Random a, Num a) => a -> Int -> Gen [a]
    splitNum _ 0 = pure []
    splitNum n size = do
      chosen <- choose (0, n)
      rest <- splitNum (n - chosen) (size - 1)
      pure $ chosen : rest

    insertAt :: Int -> a -> [a] -> [a]
    insertAt i x xs =
      let (ls, rs) = splitAt i xs
      in ls ++ [x] ++ rs

data EpochVotes = EpochVotes
  { _distribution :: [(STKR.Proposal, [SecretKey])]
  , _winner :: (STKR.Proposal, Int)
  }

makeLenses ''EpochVotes

genEpochVotes :: [STKR.Proposal] -> [SecretKey] -> Gen EpochVotes
genEpochVotes proposals councilSks = do
  (epochVotes, winnerIndex) <- votesDistribution (length councilSks) (length proposals)
  let winnerProposal =
        fromMaybe (error "unreachable: winner is always in proposal list") $
          proposals ^? ix winnerIndex
  let assignSks votesNeeded (result, availibleSks) =
        let (toBeAssigned, left) = splitAt votesNeeded availibleSks
        in (toBeAssigned : result, left)
  let votesAssignedToSks = fst $ foldr assignSks ([], councilSks) epochVotes
  pure $ EpochVotes
    { _distribution = zip proposals votesAssignedToSks
    , _winner = (winnerProposal, winnerIndex)
    }

data EpochDesc = EpochDesc
  { _num :: Natural
  , _stages :: [Timestamp]
  , _votes :: EpochVotes
  }

makeLenses ''EpochDesc

genEpochDesc :: [SecretKey] -> Integer -> Int -> Int -> Gen [EpochDesc]
genEpochDesc councilSks startYear startMonth n = do
  let epochList = take n $ govEpochsStartingAt startYear startMonth
  forM epochList $ \(epochNum, stageStarts) -> do
    proposals <- genProposals (3, 5)
    epochVotes <- genEpochVotes proposals councilSks
    pure $ EpochDesc
      { _num = epochNum
      , _stages = timestampFromUTCTime <$> stageStarts
      , _votes = epochVotes
      }
