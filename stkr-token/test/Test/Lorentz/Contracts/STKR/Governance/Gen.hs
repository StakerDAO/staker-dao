module Test.Lorentz.Contracts.STKR.Governance.Gen
  ( generateTestInput
  ) where

import Data.Time.Calendar (Day, addDays, addGregorianMonthsClip)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Lens.Micro (ix)
import System.Random (Random)
import Tezos.Core (timestampFromUTCTime)
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
    splitNum n 1 = pure [n]
    splitNum n size = do
      chosen <- choose (0, n)
      rest <- splitNum (n - chosen) (size - 1)
      pure $ chosen : rest

    insertAt :: Int -> a -> [a] -> [a]
    insertAt i x xs =
      let (ls, rs) = splitAt i xs
      in ls ++ [x] ++ rs


generateTestInput = undefined

data EpochVotes = EpochVotes
  { evVotes :: [(STKR.Proposal, [SecretKey])]
  , evWinner :: (STKR.Proposal, Int)
  }

epochVotes :: [STKR.Proposal] -> [SecretKey] -> Gen EpochVotes
epochVotes proposals councilSks = do
  (votes, winner) <- votesDistribution (length councilSks) (length proposals)
  let winnerProposal =
        fromMaybe (error "unreachable: winner is always in proposal list") $
          proposals ^? ix winner
  let assignSks votesNeeded (result, availibleSks) =
        let (toBeAssigned, left) = splitAt votesNeeded availibleSks
        in (toBeAssigned : result, left)
  let votesAssignedToSks = fst $ foldr assignSks ([], councilSks) votes
  pure $ EpochVotes
    { evVotes = zip proposals votesAssignedToSks
    , evWinner = (winnerProposal, winner)
    }
