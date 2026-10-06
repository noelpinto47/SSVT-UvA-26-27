module Exercise3 where

import Test.QuickCheck (generate)
import Mutation (mutators)
import MultiplicationTable (multiplicationTable, multiplicationTableProps)
import Control.Monad (replicateM)
import Data.List (minimumBy, subsequences)
import Data.Ord (comparing)

-- Find the minimal subset of properties that keeps the full kill set.
--   fut   : the function under test
--   props : the properties
--   n     : mutants generated per mutator
findMinimalSubset :: (a -> [Integer]) -> a -> [[Integer] -> a -> Bool] -> Int -> IO [[Integer] -> a -> Bool]
findMinimalSubset fut input props n = do
    let original = fut input

    -- One shared pool of mutants, so every subset is judged on the same mutants.
    pool <- concat <$> mapM (\mutator -> replicateM n (generate (mutator original))) mutators

    -- Outputs identical to the original are not mutations, so they are dropped.
    let muts = filter (/= original) pool

        -- A property kills a mutant if it returns False on it
        kills prop output = not (prop output input)

        -- Benchmark: mutants killed by at least one property in the full set
        fullKill = filter (\m -> any (\p -> kills p m) props) muts

        -- A subset covers the benchmark if it kills every mutant in fullKill
        covers subset = all (\m -> any (\p -> kills p m) subset) fullKill

        -- Valid subsets, from which we pick the smallest
        candidates = [ subset | subset <- subsequences props, not (null subset), covers subset ]

    putStrLn $ "Mutants in pool      : " ++ show (length muts)
    putStrLn $ "Full kill set size   : " ++ show (length fullKill)
    putStrLn $ "Subsets covering it  : " ++ show (length candidates)

    return (minimumBy (comparing length) candidates)

main :: IO ()
main = do
    -- implementation of minimal subset that takes fut, input, properties and n as parameters
    minimal <- findMinimalSubset multiplicationTable 10 multiplicationTableProps 10000

    putStrLn "\n═══ Result ═══"
    putStrLn $ "  Minimal subset size: " ++ show (length minimal)
