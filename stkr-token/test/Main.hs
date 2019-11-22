module Main
  ( main
  ) where

import Test.Tasty (defaultMainWithIngredients)

import Util.Test.Ingredients (ourIngredients)

import Tree (tests)

main :: IO ()
main = tests >>= defaultMainWithIngredients ourIngredients
