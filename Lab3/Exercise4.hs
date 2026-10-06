module Exercise4 where

import Test.QuickCheck
import Data.List
import Data.Maybe
import Debug.Trace

import MultiplicationTable
import Mutation

{-
Exercise 4
Implement a function that calculates the
strength of a given set of properties, which
is the percentage of mutants they kill.
-}

-- EXERCISE 4
-- Time spent: 3 hours

-- mutator changes output of function under test
type Mutator a = a -> Gen a

-- property takes mutated output and input and checks if it is correct
type Prop a = a -> Integer -> Bool

-- function under test takes input and returns output
type FunctionUnderTest a = Integer -> a

-- generates a number of mutants, each with a random mutator and input
-- returns the property results for each mutant
mutantResults :: Eq a => Int -> [Mutator a] -> [Prop a] -> FunctionUnderTest a -> Gen [[Bool]]
mutantResults numMutants mutators properties functionUnderTest = vectorOf numMutants $ do
  mutator <- elements mutators
  input <- arbitrary `suchThat` (> 0)
  mutate' mutator properties functionUnderTest input

-- a mutant is killed if atleast one property fails for it
isKilled :: [Bool] -> Bool
isKilled = not . and

-- calculate the percentage of mutants killed by the properties
strength :: Eq a => Int -> [Mutator a] -> [Prop a] -> FunctionUnderTest a -> Gen Double
strength numMutants mutators properties functionUnderTest = do
    results <- mutantResults numMutants mutators properties functionUnderTest
    let valid = filter (not . null) results
        numKilled = length (filter isKilled valid)
    return $ if null valid
        then 0
        else 100 * fromIntegral numKilled / fromIntegral (length valid)

-- main function to run the strength calculation and print the result
main :: IO ()
main = do
    score <- generate $ strength 1000 [addElements, removeElements, anyList] multiplicationTableProps multiplicationTable
    putStrLn $ "Strength of properties: " ++ show score ++ "%"