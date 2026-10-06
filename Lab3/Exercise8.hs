module Exercise3 where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Mutation
import MultiplicationTable(multiplicationTable, multiplicationTableProps, prop_tenElements)
import Control.Monad (replicateM, forM)
import Data.List (minimumBy, subsequences)
import Data.Ord (comparing)

-- minimal property subset is the smallest combination
-- of your defined properties that maintains the highest possible mutation score
-- (the percentage of killed mutants)

-- Finding this subset involves:
-- Mutant-Property Matrix:
-- Tracking which specific properties kill which specific mutants.
-- Set Covering Algorithm:
-- Mathematically finding the smallest combination of properties
-- that covers all killable mutants.

-- Define a simple function under test (doubles the input)
fut :: Integer -> Integer
fut x = x * 2

-- Define a property (checks if the result is even)
prop :: Integer -> Integer -> Bool
prop res _ = even res

-- Define a mutator (corrupts the output by adding 1)
mutator :: Integer -> Gen Integer
mutator x = return (x + 1)

-- Run the mutate function with an input of 10
exampleRun :: Gen (Maybe Bool)
exampleRun = mutate mutator prop fut 10

-- exampleRun' with prop_tenElements, returns [Bool] ([False]=killed, [True]=survived, []=no change)
exampleRun' :: Gen ([Bool])
exampleRun' = mutate' addElements [prop_tenElements] multiplicationTable 10

checkingSingleProperty = mutate addElements prop_tenElements multiplicationTable 10

-- replicateM n action runs an IO action n times and collects results into a list
-- so you'd get [Maybe Bool] with 100 results.
-- Maybe Bool. In Haskell, Nothing typically means the computation produced no result.

-- Just False = mutant killed
-- Just True = mutant survived
-- Nothing = ignore (no valid mutation happened)

-- takes n as parameter instead of hardcoding 10000
runAnalysis :: Int -> IO ()
runAnalysis n = do
  putStrLn $ "\n── Single Property Analysis (prop_tenElements, n=" ++ show n ++ ") ──"
  results <- replicateM n (generate exampleRun')

  let indexed  = zip [0..] results
      killed   = filter (\(_, r) -> r == [False]) indexed
      survived = filter (\(_, r) -> r == [True])  indexed
      noChange = filter (\(_, r) -> r == [])       indexed

  putStrLn $ "  Total     : " ++ show (length results)
  putStrLn $ "  Killed    : " ++ show (length killed)
  putStrLn $ "  Survived  : " ++ show (length survived)
  putStrLn $ "  No change : " ++ show (length noChange)
  putStrLn $ "  Survivor indices: " ++ show (map fst survived)

  putStrLn "\n  Example surviving mutant output:"
  exampleSurvivor <- generate (addElements (multiplicationTable 10))
  print exampleSurvivor


-- Run n mutant instances for a given set of properties, return killed indices
killedBy :: [[Integer] -> Integer -> Bool] -> Int -> IO [Int]
killedBy props n = do
    results <- replicateM n (generate (mutate' addElements props multiplicationTable 10))
    return [ i | (i, r) <- zip [0..] results, r == [False] ]

-- Compute the full kill set using all properties (the benchmark)
fullKillSet :: Int -> IO [Int]
fullKillSet n = killedBy multiplicationTableProps n

-- Check if a subset's kill set covers the full kill set
covers :: [Int] -> [Int] -> Bool
covers fullSet subSet = all (`elem` subSet) fullSet

-- Find the minimal subset of properties that kills all mutants
-- killed by the full property set
findMinimalSubset :: Int -> IO [[Integer] -> Integer -> Bool]
findMinimalSubset n = do
    -- Run single-property analysis first for context
    runAnalysis n

    putStrLn "\n── Computing Full Kill Set (all properties) ──"
    full <- fullKillSet n
    putStrLn $ "  Full kill set size: " ++ show (length full)

    let subsets = filter (not . null) (subsequences multiplicationTableProps)
    putStrLn $ "\n── Checking " ++ show (length subsets) ++ " non-empty subsets ──"

    results <- forM (zip [1..] subsets) (\(idx, subset) -> do
        killed <- killedBy subset n
        -- log each subset's result
        let score = length killed * 100 `div` n
        putStrLn $ "  Subset " ++ show (idx :: Int)
                ++ " (size " ++ show (length subset) ++ ")"
                ++ " → killed " ++ show (length killed)
                ++ " (" ++ show score ++ "%)"
                ++ if covers full killed then " covers full set" else ""
        return (subset, killed))

    let covering = filter (\(_, killed) -> covers full killed) results

    -- log how many subsets cover the full kill set
    putStrLn $ "\n  Subsets covering full kill set: " ++ show (length covering)

    let minimal = minimumBy (comparing (length . fst)) covering
    return (fst minimal)

main :: IO ()
main = do
    let n = 10000

    minimal <- findMinimalSubset n

    -- Final summary
    putStrLn "\n═══ Result ═══"
    putStrLn $ "  Minimal subset size: " ++ show (length minimal)
