module Exercise2 where

import Data.List
import Mutation (mutate', anyList, removeElements, addElements)
import Exercise1 (permutate, allElementsBecomeFirstElement, reversesList)
import Multiplication (multiplicationTableProps, multiplicationTable)
import Test.QuickCheck (Gen, shuffle, generate)
import System.Random (randomIO, randomRIO)

main = do
  let nGenuineMutants = 100
  print ("Test 1: All mutants against all props, nGenuineMutants: 100" )
  countTest1 <- countSurvivors nGenuineMutants mutators multiplicationTableProps multiplicationTable
  print ("Test 1: Number of survivors: " ++ show countTest1)
  
  print ("Test 2: All mutants against just one prop, nGenuineMutants: 100" )
  countsTest2 <- mapM (\prop -> countSurvivors nGenuineMutants mutators [prop] multiplicationTable) multiplicationTableProps
  print "Test2: Number of survivors per property"
  print countsTest2

  
countSurvivors :: Integer -> [([Integer] -> Gen [Integer])] -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> IO Integer
countSurvivors nGenuineMutants mutators props fut = do
  statuses <- loop 0 []
  -- print statuses
  let survivorCount = fromIntegral (length (filter (== Survived) statuses))
  return survivorCount
  where
    loop nGeneratedMutants statuses
      | nGeneratedMutants >= nGenuineMutants = return statuses
      | otherwise = do
          randInt <- randomIO :: IO Integer
          -- print("Picking random input integer: " ++ show randInt)
          (mutatorIndex, randMutator) <- randomElementWithIndex mutators
          -- print("Picking mutator number: " ++ show (mutatorIndex + 1))
          intermediateResult <-generate $ mutate' randMutator props fut randInt
          -- print("Result per property of testing on input and mutator:" ++ show intermediateResult)
          let classification = classifyResults intermediateResult
          -- print("Final Result for Mutant:" ++ show classification)
          -- putStrLn ""
          case classification of
            Unchanged -> loop nGeneratedMutants statuses
            status -> loop (nGeneratedMutants + 1) (status : statuses)
      
randomElementWithIndex :: [a] -> IO (Int, a)
randomElementWithIndex [] = error "Cannot pick from an empty list"
randomElementWithIndex xs = do
  idx <- randomRIO (0, length xs - 1)
  return (idx, xs !! idx)
  
data MutationStatus
  = Unchanged
  | Survived
  | Killed
  deriving (Show, Eq)

classifyResults :: [Bool] -> MutationStatus
classifyResults [] = Unchanged
classifyResults results
  | and results = Survived
  | otherwise   = Killed

mutators = [anyList, removeElements, addElements, permutate, allElementsBecomeFirstElement, reversesList]
