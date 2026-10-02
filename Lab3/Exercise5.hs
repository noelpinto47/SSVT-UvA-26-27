module Exercise5 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import Control.Monad (replicateM_)

calculateEquivalentConjectureOneMutator :: 
  [[Integer] -> Integer -> Bool] -> 
  [[Integer] -> Integer -> Bool] ->
  ([Integer] -> Gen [Integer]) -> 
  IO Bool
calculateEquivalentConjectureOneMutator setA setB mutator = do
    random_number <- generate arbitrary
    let original_output_list = multiplicationTable random_number
    mutant <- generate (mutator original_output_list)
    let setAResults = map (\property -> property mutant random_number) setA
    let setBResults = map (\property -> property mutant random_number) setB
    let setA_survived = and setAResults
    let setB_survived = and setBResults
    return (setA_survived == setB_survived)

calculateEquivalentConjectureAllMutators :: 
  [[Integer] -> Integer -> Bool] -> 
  [[Integer] -> Integer -> Bool] ->
  [([Integer] -> Gen [Integer])] ->
  IO [Bool]
calculateEquivalentConjectureAllMutators setA setB mutatorsList = do
    -- calculateEquivalentConjectureOneMutator is returning a 'monadic' IO Bool, not normal Bool
    -- that is why we need mapM instead of map
    results <- mapM (calculateEquivalentConjectureOneMutator setA setB) mutatorsList    
    return results

calculateImplicationConjectureOneMutator :: 
  [[Integer] -> Integer -> Bool] -> 
  [[Integer] -> Integer -> Bool] ->
  ([Integer] -> Gen [Integer]) -> 
  IO Bool
calculateImplicationConjectureOneMutator setA setB mutator = do
    random_number <- generate arbitrary
    let original_output_list = multiplicationTable random_number
    mutant <- generate (mutator original_output_list)
    let setAResults = map (\property -> property mutant random_number) setA
    let setBResults = map (\property -> property mutant random_number) setB
    let setA_survived = and setAResults
    let setB_survived = and setBResults

    let result = setA_survived MultiplicationTable.--> setB_survived
    return result

calculateImplicationConjectureAllMutators :: 
  [[Integer] -> Integer -> Bool] -> 
  [[Integer] -> Integer -> Bool] ->
  [([Integer] -> Gen [Integer])] ->
  IO [Bool]
calculateImplicationConjectureAllMutators setA setB mutatorsList = do
    results <- mapM (calculateImplicationConjectureOneMutator setA setB) mutatorsList    
    return results

calculateDisjointConjectureOneMutator :: 
  [[Integer] -> Integer -> Bool] -> 
  [[Integer] -> Integer -> Bool] ->
  ([Integer] -> Gen [Integer]) -> 
  IO Bool
calculateDisjointConjectureOneMutator setA setB mutator = do
    random_number <- generate arbitrary
    let original_output_list = multiplicationTable random_number
    mutant <- generate (mutator original_output_list)
    let setAResults = map (\property -> property mutant random_number) setA
    let setBResults = map (\property -> property mutant random_number) setB
    let setA_survived = and setAResults
    let setB_survived = and setBResults

    let killedByA = not setA_survived
    let killedByB = not setB_survived
    return (not (killedByA && killedByB))

calculateDisjointConjectureAllMutators :: 
  [[Integer] -> Integer -> Bool] -> 
  [[Integer] -> Integer -> Bool] ->
  [([Integer] -> Gen [Integer])] ->
  IO [Bool]
calculateDisjointConjectureAllMutators setA setB mutatorsList = do
    results <- mapM (calculateDisjointConjectureOneMutator setA setB) mutatorsList    
    return results

checkAllConjecturesWithShuffle :: [[Integer] -> Integer -> Bool] -> IO ()
checkAllConjecturesWithShuffle wholePropertyList = do
    shuffled <- generate (shuffle wholePropertyList)
    splitIndex <- generate (choose (1, length shuffled - 1))
    let (setA, setB) = splitAt splitIndex shuffled

    putStrLn "\nResults from one shuffle run:"
    result_equivalent_all_mutators <- calculateEquivalentConjectureAllMutators setA setB Mutation.mutators
    print (zipWith (\name result -> (name, result)) ["anyList", "removeElements", "addElements"] result_equivalent_all_mutators)
    
    result_implication_all_mutators <- calculateImplicationConjectureAllMutators setA setB Mutation.mutators
    print (zipWith (\name result -> (name, result)) ["anyList", "removeElements", "addElements"] result_implication_all_mutators)
    
    result_disjoint_all_mutators <- calculateDisjointConjectureAllMutators setA setB Mutation.mutators
    print (zipWith (\name result -> (name, result)) ["anyList", "removeElements", "addElements"] result_disjoint_all_mutators)


checkAllConjecturesNTimes :: Int -> [[Integer] -> Integer -> Bool] -> IO ()
checkAllConjecturesNTimes input wholePropertyList = replicateM_ input (checkAllConjecturesWithShuffle wholePropertyList)

main :: IO ()
main = do
  let setA =  
        [ 
            MultiplicationTable.prop_tenElements, 
            MultiplicationTable.prop_firstElementIsInput 
        ]

  let setB = 
        [ 
            MultiplicationTable.prop_sumIsTriangleNumberTimesInput, 
            MultiplicationTable.prop_linear, 
            MultiplicationTable.prop_moduloIsZero
        ]

  -- Equivalent
  result_equivalent_anyList_mutator <- calculateEquivalentConjectureOneMutator setA setB anyList
  putStrLn "\nResults result_equivalent_anyList_mutator:"
  print result_equivalent_anyList_mutator

  result_equivalent_all_mutators <- calculateEquivalentConjectureAllMutators setA setB Mutation.mutators
  putStrLn "\nResults result_equivalent_all_mutators:"
  print (zipWith (\name result -> (name, result)) ["anyList", "removeElements", "addElements"] result_equivalent_all_mutators)

  -- Implication
  result_implication_anyList_mutator <- calculateImplicationConjectureOneMutator setA setB anyList
  putStrLn "\nResults result_implication_anyList_mutator:"
  print result_implication_anyList_mutator

  result_implication_all_mutators <- calculateImplicationConjectureAllMutators setA setB Mutation.mutators
  putStrLn "\nResults result_implication_all_mutators:"
  print (zipWith (\name result -> (name, result)) ["anyList", "removeElements", "addElements"] result_implication_all_mutators)

  -- Disjoint
  result_disjoint_anyList_mutator <- calculateDisjointConjectureOneMutator setA setB anyList
  putStrLn "\nResults result_disjoint_anyList_mutator:"
  print result_disjoint_anyList_mutator

  result_disjoint_all_mutators <- calculateDisjointConjectureAllMutators setA setB Mutation.mutators
  putStrLn "\nResults result_disjoint_all_mutators:"
  print (zipWith (\name result -> (name, result)) ["anyList", "removeElements", "addElements"] result_disjoint_all_mutators)


-- Until now, always the same property sets got used
-- It would be good, if we can mix it up
-- The properties are functions and functions do not have an Eq instance
-- Therefore, we shuffle and just split the list in two

  checkAllConjecturesNTimes 4 MultiplicationTable.multiplicationTableProps