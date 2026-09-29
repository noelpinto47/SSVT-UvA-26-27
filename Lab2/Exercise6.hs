module Exercise6 where

import Data.List
import System.Random
import Test.QuickCheck

import Exercise3 (symClos)
import Exercise5 (trClos, Rel)

main :: IO ()
main = do
    putStrLn("Testing symClos on Int domain:")
    mapM_ (\r -> print (r, symClos r)) intDomain

    putStrLn("QuickCheck tests on symClos properties:")
    -- quickCheck prop1_SymCloseSizeIsGreaterOrEqual
    quickCheck prop2_property_symmetricElementShouldExist
    quickCheck prop3_property_originalPairsRemain
    quickCheck prop4_property_idempotentSymClos
    quickCheck prop5_property_noDuplicatePairs

    putStrLn("QuickCheck tests on symClos properties with random sets:")
    -- quickCheck prop1_SymCloseSizeIsGreaterOrEqual_random
    quickCheck prop2_property_symmetricElementShouldExist_random
    quickCheck prop3_property_originalPairsRemain_random
    quickCheck prop4_property_idempotentSymClos_random
    quickCheck prop5_property_noDuplicatePairs_random

    putStrLn("QuickCheck tests on trClos properties:")
    quickCheck prop1_property_transitiveElementShouldExist
    quickCheck prop2_property_idempotentTransClos
    quickCheck prop3_property_noDuplicatePairsTransClos

    putStrLn("QuickCheck tests on trClos properties with random sets:")
    quickCheck prop1_property_transitiveElementShouldExist_random
    quickCheck prop2_property_idempotentTransClos_random
    quickCheck prop3_property_noDuplicatePairsTransClos_random


-- --QuickCheck Properties of the symmetric closure function
-- prop1_SymCloseSizeIsGreaterOrEqual :: Rel Int -> Bool
-- prop1_SymCloseSizeIsGreaterOrEqual r = length (symClos r) >= (length r)

--Every pair in the symmetric closure has its symmetric counterpart.
prop2_property_symmetricElementShouldExist :: Rel Int -> Bool
prop2_property_symmetricElementShouldExist r = all (\(x,y) -> (y,x) `elem` symClos r) r

--Every pair in the original relation remains in the result.
prop3_property_originalPairsRemain :: Rel Int -> Bool
prop3_property_originalPairsRemain r = all (`elem` symClos r) r

--Applying symmetric closure twice changes nothing further:
prop4_property_idempotentSymClos :: Rel Int -> Bool
prop4_property_idempotentSymClos r = symClos (symClos r) == symClos r

--The result should not contain duplicate pairs.
prop5_property_noDuplicatePairs :: Rel Int -> Bool
prop5_property_noDuplicatePairs r = length (symClos r) == length (nub (symClos r))

--Same properties with randomly generated sets of integers
-- prop1_SymCloseSizeIsGreaterOrEqual_random :: Property
-- prop1_SymCloseSizeIsGreaterOrEqual_random = forAll genSmallIntList prop1_SymCloseSizeIsGreaterOrEqual

prop2_property_symmetricElementShouldExist_random :: Property
prop2_property_symmetricElementShouldExist_random = forAll genSmallIntList prop2_property_symmetricElementShouldExist

prop3_property_originalPairsRemain_random :: Property
prop3_property_originalPairsRemain_random = forAll genSmallIntList prop3_property_originalPairsRemain

prop4_property_idempotentSymClos_random :: Property
prop4_property_idempotentSymClos_random = forAll genSmallIntList prop4_property_idempotentSymClos

prop5_property_noDuplicatePairs_random :: Property
prop5_property_noDuplicatePairs_random = forAll genSmallIntList prop5_property_noDuplicatePairs

intDomain :: [Rel Int]
intDomain = [[(1,2), (2,3), (3,4)]
            ,[(1,2), (1,2)]]
            
-- []
-- [(1,1)]
-- [(1,2)]
-- [(1,2),(2,1)]
-- [(1,2),(1,2)]
-- [(1,2),(2,3),(3,4)]

-- generates a random set of integers of random size n(between 0 and 10) with elements between -10 and 10
genSmallIntList :: Gen (Rel Int)
genSmallIntList = do
  n <- choose (0, 10)
  vectorOf n $ do
    x <- choose (-10, 10)
    y <- choose (-10, 10)
    return (x, y)

-- QuickCheck properties for transitive closure
--Every pair in the transitive closure should have its transitive counterpart.
prop1_property_transitiveElementShouldExist :: Rel Int -> Bool
prop1_property_transitiveElementShouldExist r = all (\(x,y) -> all (\(y',z) -> y /= y' || (x,z) `elem` trClos r) r) (trClos r)

--Applying transitive closure twice changes nothing further:
prop2_property_idempotentTransClos :: Rel Int -> Bool
prop2_property_idempotentTransClos r = trClos (trClos r) == trClos r

--The result should not contain duplicate pairs.
prop3_property_noDuplicatePairsTransClos :: Rel Int -> Bool
prop3_property_noDuplicatePairsTransClos r = length (trClos r) == length (nub (trClos r))

--Same properties with randomly generated sets of integers
prop1_property_transitiveElementShouldExist_random :: Property
prop1_property_transitiveElementShouldExist_random = forAll genSmallIntList prop1_property_transitiveElementShouldExist

prop2_property_idempotentTransClos_random :: Property
prop2_property_idempotentTransClos_random = forAll genSmallIntList prop2_property_idempotentTransClos

prop3_property_noDuplicatePairsTransClos_random :: Property
prop3_property_noDuplicatePairsTransClos_random = forAll genSmallIntList prop3_property_noDuplicatePairsTransClos