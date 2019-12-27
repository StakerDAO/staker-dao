{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Lorentz.Contracts.TestCommon
  ( spec_listAt
  ) where

import Data.Functor.Identity (Identity (..))
import Lens.Micro (ix)

import Test.Hspec (Spec, it)
import Test.HUnit ((@?=))
import qualified Test.Hspec.QuickCheck as HQ
import Test.QuickCheck (Arbitrary(..), choose)

import Michelson.Interpret (MichelsonFailed)

import qualified Lorentz as L
import Lorentz.Test

import Lorentz.Contracts.Common

data ListAtTest = ListAtTest [Integer] Natural (Maybe Integer)
  deriving Show

instance Arbitrary ListAtTest where
  arbitrary = do
    l <- arbitrary
    let len = length l
    i <- choose (0, len*2)
    pure $ ListAtTest l (fromIntegral i) (l ^? ix i)

spec_listAt :: Spec
spec_listAt = do
  it "Out of bounds (1)" $ run 4 [1, 2, 3] @?= Right Nothing
  it "Out of bounds (2)" $ run 3 [1, 2, 3] @?= Right Nothing
  it "First index" $ run 0 [1, 2, 3, 4] @?= Right (Just 1)
  it "Last index" $ run 3 [1, 2, 3, 4] @?= Right (Just 4)
  it "Mid index" $ run 2 [1, 2, 3, 4] @?= Right (Just 3)
  HQ.prop "Random test" $ \(ListAtTest l i r) ->
    run i l @?= Right r
  where
    run :: Natural -> [Integer] -> Either MichelsonFailed (Maybe Integer)
    run i l = do
      let initStack = (Identity l L.:& Identity i L.:& L.RNil)
      resStack <- L.interpretLorentzInstr dummyContractEnv listAt initStack
      case resStack of Identity mEl L.:& L.RNil -> return mEl
