module Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- EXERCISE 1
-- TIME SPENT: 4 hours

-- checks if the elements of the set are in increasing order
invariant :: Ord a => Set a -> Bool
invariant (Set elements) = and (zipWith (<) elements (drop 1 elements))

-- generates a random set of integers with size 0-20, and values (-10)-10
randomSet :: IO (Set Int)
randomSet = do
    size <- randomRIO (0, 20)
    randomValues <- sequence (replicate size (randomRIO (-10, 10)))
    return (list2set randomValues)

-- tests the invariant on a number of randomly generated sets
testScratch :: Int -> IO ()
testScratch numSets = do
    generatedSets <- sequence (replicate numSets randomSet)
    mapM_ print (take 5 generatedSets)
    putStrLn ("Invariant holds for all " ++ show numSets ++ " sets: " ++ show (all invariant generatedSets))

-- Arbitrary instance for Set a, using the list2set function to convert a list to a set
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = list2set <$> arbitrary
    shrink (Set elements) = map list2set (shrink elements)

-- Property tests for the Set data structure
prop_generatedSetIsValid :: Set Int -> Bool
prop_generatedSetIsValid = invariant

-- Property test to check that all shrunk sets are valid
prop_shrunkSetsAreValid :: Set Int -> Bool
prop_shrunkSetsAreValid set = all invariant (shrink set)

-- Main function to run the tests
main :: IO ()
main = do
    testScratch 100
    sample (arbitrary :: Gen (Set Int))
    quickCheck prop_generatedSetIsValid
    quickCheck prop_shrunkSetsAreValid