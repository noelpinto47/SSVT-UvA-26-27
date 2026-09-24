module Main where

import Test.QuickCheck
import Exercise1

-- main function to run the property tests
main :: IO ()
main = do
      quickCheck prop_factorialPositive
      quickCheck prop_factorialRecursiveIdentity
      quickCheck prop_factorialIncreasing