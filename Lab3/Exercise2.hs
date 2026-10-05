import Data.List
import Test.QuickCheck
import Control.Monad (foldM)
import Mutation (mutate', mutators)
import Multiplication (multiplicationTableProps, multiplicationTableProps)
import Test.QuickCheck (Gen, shuffle, generate)


main = do
  let input = [1,2,3,4]
  count <- countSurvivors mutators multiplicationTableProps multiplicationTable input
  print ("Number of survivors: " ++ show count)
  
countSurvivors :: [([Integer] -> Gen [Integer])] -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> [Integer] -> IO Integer
countSurvivors mutators props fut inputs = do
  resultsPerInputPerMutator <- mapM (\input -> mapM (\mutator -> generate $ mutate' mutator props fut input) mutators) inputs
  printMutationResults inputs resultsPerInputPerMutator
  let statuses = map (map classifyResults) resultsPerInputPerMutator
  let survivorCount = fromIntegral (length (filter (== Survived) (concat statuses)))
  return survivorCount

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
  
printMutationResults :: [Integer] -> [[[Bool]]] -> IO ()
printMutationResults inputs resultsPerInputPerMutator =
  forM_ (zip inputs resultsPerInputPerMutator) $ \(input, resultsPerMutator) -> do
    putStrLn $ "Input: " ++ show input

    forM_ (zip [1..] resultsPerMutator) $ \(mutatorNumber, propertyResults) -> do
      putStrLn $
        "  Mutator " ++ show mutatorNumber
        ++ ": " ++ show propertyResults
        ++ " -> " ++ show (classifyResults propertyResults)