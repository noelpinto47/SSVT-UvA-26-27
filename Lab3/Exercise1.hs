import Data.List
import Test.QuickCheck
import Mutation

import Test.QuickCheck (Gen, shuffle, generate)

main = do
  let list = [1,2,3,4,5]
  shuffled <- generate (permutate list)
  print shuffled
  firstElement <- generate (allElementsBecomeFirstElement list)
  print firstElement
  reversed <- generate (reversesList list)
  print reversed

-- | Generates a random permutation of xs inside the QuickCheck Gen monad
permutate :: [a] -> Gen [a]
permutate xs = shuffle xs

allElementsBecomeFirstElement :: [a] -> Gen [a]
allElementsBecomeFirstElement xs = case xs of
  [] -> return []
  (y:ys) -> return $ replicate (length xs) y

--other mutators
reversesList :: [a] -> Gen [a]
reversesList xs = return $ reverse xs