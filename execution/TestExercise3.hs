module Main where

import Test.QuickCheck
import Exercise3
import Lecture3 (exercise)

main :: IO ()
main = do
    putStrLn (exercise 3 "Testing Properties Strength")

    putStrLn ""
    putStrLn "Domain:"
    print domain

    putStrLn ""
    putStrLn "=== Strength Results ==="

    putStrLn "1. (even x && x > 3) is stronger than even:"
    print test1

    putStrLn "2. even is stronger than (even x || x > 3):"
    print test2

    putStrLn "3. ((even x && x > 3) || even x) is stronger than even:"
    print test3a

    putStrLn "4. even is stronger than ((even x && x > 3) || even x):"
    print test3b

    putStrLn ""
    putStrLn "=== QuickCheck ==="

    quickCheck prop_test1
    quickCheck prop_test2
    quickCheck prop_test3a
    quickCheck prop_test3b

    putStrLn ""
    putStrLn "=== Answer ==="
    putStrLn ""
    putStrLn "1. LEFT is stronger."
    putStrLn "2. RIGHT is stronger."
    putStrLn "3. Both are equally strong."
    putStrLn "4. Both are equally strong."

    putStrLn ""
    putStrLn "=== Strength ordering ==="
    putStrLn ""
    putStrLn "prop1 > prop4 = prop3 > prop2"
    putStrLn "where prop3 and prop4 are equivalent."