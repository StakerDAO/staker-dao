module Main
  (
    main
  ) where

import Lorentz (printLorentzContract)
import Lorentz.Contracts.STKR (stkrContract)

main :: IO ()
main =
  putStrLn $ printLorentzContract False stkrContract
