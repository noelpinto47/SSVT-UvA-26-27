module Main where

import Exercise4
import Test.QuickCheck

-- main function to run the property tests
main :: IO ()
main = do
    quickCheck prop_permReflexive
    quickCheck prop_permSymmetric
    quickCheck prop_permSameLength
    quickCheck prop_permKnown