module Exercise4 where

-- CHECKS IF TWO LISTS ARE PERMUTATIONS OF EACH OTHER

-- EXERCISE 4
-- TIME SPENT: 3.5 hours

import Data.List (sort, nub)
import Test.QuickCheck

-- two lists are permutations of each other if they contain the same elements, even in a different order
isPermutation :: (Ord a) => [a] -> [a] -> Bool
isPermutation list1 list2 = sort list1 == sort list2

-- generator for lists of integers without duplicates
-- nub <$> arbitrary generates a list of integers and removes duplicates
genNoDups :: Gen [Int]
genNoDups = nub <$> arbitrary





-- property 1: a list is always a permutation of itself
prop_permReflexive :: Property
prop_permReflexive = forAll genNoDups $ \list1 -> isPermutation list1 list1


-- property 2: permutation is symmetric
-- if list1 is a permutation of list2, then list2 is a permutation of list1
prop_permSymmetric :: Property
prop_permSymmetric = forAll genNoDups $ \list1 -> forAll (shuffle list1) $ \list2 -> isPermutation list1 list2 == isPermutation list2 list1


-- property 3: if two lists are permutations of each other, they must have the same length
prop_permSameLength :: Property
prop_permSameLength = forAll genNoDups $ \list1 -> forAll (shuffle list1) $ \list2 -> isPermutation list1 list2 ==> length list1 == length list2


-- property 4: if shuffle a list, it should be a permutation of that original list
prop_permKnown :: Property
prop_permKnown = forAll genNoDups $ \list1 -> forAll (shuffle list1) $ \list2 -> isPermutation list1 list2 == (sort list1 == sort list2)




-- default value for lists
orDefault :: [a] -> [a] -> [a]
orDefault [] list2 = list2
orDefault list1 _  = list1

-- logical implication operator
-- p --> q is equivalent to (not p) || q
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- main function to run the property tests
main :: IO ()
main = do
    quickCheck prop_permReflexive
    quickCheck prop_permSymmetric
    quickCheck prop_permSameLength
    quickCheck prop_permKnown
