module Main where

import Test.QuickCheck
import Exercise8
import Lecture3 (exercise)

main :: IO ()
main = do
    putStrLn (exercise 8 "Sub-formulae (sub, nsub)")
 
    putStrLn "-- sub --"
    quickCheck prop_subContainsItself
    quickCheck prop_subCorrect
 
    putStrLn ""
    putStrLn "-- nsub --"
    quickCheck prop_nsubMatchesSub
    quickCheck prop_nsubPositive
