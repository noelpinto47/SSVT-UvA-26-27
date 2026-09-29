module Exercise6 where

import Data.List
import System.Random
import Test.QuickCheck

import Exercise3 (symClos)
import Exercise5 (trClos, Rel)

main :: IO ()
main = do
    putStrLn("Testing symClos on Int domain([Input], [Symmetric Closure Output], [Transitive Closure Output]):")
    mapM_ (\r -> print (r, symClos r, trClos r)) intDomain
    putStrLn("Testing symClos on Char domain([Input], [Symmetric Closure Output], [Transitive Closure Output]):")
    mapM_ (\r -> print (r, symClos r, trClos r)) charDomain
    putStrLn("Testing symClos on String domain([Input], [Symmetric Closure Output], [Transitive Closure Output]):")
    mapM_ (\r -> print (r, symClos r, trClos r)) stringDomain
    putStrLn("Testing symClos on Bool domain([Input], [Symmetric Closure Output], [Transitive Closure Output]):")
    mapM_ (\r -> print (r, symClos r, trClos r)) boolDomain
    

    putStrLn("QuickCheck tests on symClos properties(excluding prop1_SymCloseSizeIsGreaterOrEqual, prop5_property_noDuplicatePairs)")
    quickCheck prop2_property_symmetricElementShouldExist
    quickCheck prop3_property_originalPairsRemain
    quickCheck prop4_property_idempotentSymClos

    putStrLn("QuickCheck tests using custom generator genSmallIntList on symClos properties:")
    quickCheck prop1_SymCloseSizeIsGreaterOrEqual_random
    quickCheck prop2_property_symmetricElementShouldExist_random
    quickCheck prop3_property_originalPairsRemain_random
    quickCheck prop4_property_idempotentSymClos_random
    quickCheck prop5_property_noDuplicatePairs_random

    putStrLn("QuickCheck tests on trClos properties(excluding prop3_property_noDuplicatePairsTransClos):")
    quickCheck prop1_property_transitiveElementShouldExist
    quickCheck prop2_property_idempotentTransClos
    quickCheck prop4_property_TrCloseSizeGreaterOrEqual
    quickCheck prop5_property_TrCloseContainsOriginal

    putStrLn("QuickCheck tests on trClos properties with random sets:")
    quickCheck prop1_property_transitiveElementShouldExist_random
    quickCheck prop2_property_idempotentTransClos_random
    quickCheck prop3_property_noDuplicatePairsTransClos_random
    quickCheck prop4_property_TrCloseSizeGreaterOrEqual_random
    quickCheck prop5_property_TrCloseContainsOriginal_random


-- --QuickCheck Properties of the symmetric closure function
-- This is run only on generator because QuickCheck might create duplicate elements in input.
-- Rest of the properties can be run using quickcheck input generation.
prop1_SymCloseSizeIsGreaterOrEqual :: Rel Int -> Bool
prop1_SymCloseSizeIsGreaterOrEqual r = length (symClos r) >= (length r)

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

--Same properties with randomly generated sets of integers - these lists are ordered(sorted) and do NOT contain duplicates
prop1_SymCloseSizeIsGreaterOrEqual_random :: Property
prop1_SymCloseSizeIsGreaterOrEqual_random = forAll genSmallIntList prop1_SymCloseSizeIsGreaterOrEqual

prop2_property_symmetricElementShouldExist_random :: Property
prop2_property_symmetricElementShouldExist_random = forAll genSmallIntList prop2_property_symmetricElementShouldExist

prop3_property_originalPairsRemain_random :: Property
prop3_property_originalPairsRemain_random = forAll genSmallIntList prop3_property_originalPairsRemain

prop4_property_idempotentSymClos_random :: Property
prop4_property_idempotentSymClos_random = forAll genSmallIntList prop4_property_idempotentSymClos

prop5_property_noDuplicatePairs_random :: Property
prop5_property_noDuplicatePairs_random = forAll genSmallIntList prop5_property_noDuplicatePairs



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

prop4_property_TrCloseSizeGreaterOrEqual :: Rel Int -> Bool
prop4_property_TrCloseSizeGreaterOrEqual r = length (trClos r) >= length r

prop5_property_TrCloseContainsOriginal :: Rel Int -> Bool
prop5_property_TrCloseContainsOriginal r = all (`elem` trClos r) r

--Same properties with randomly generated sets of integers
prop1_property_transitiveElementShouldExist_random :: Property
prop1_property_transitiveElementShouldExist_random = forAll genSmallIntList prop1_property_transitiveElementShouldExist

prop2_property_idempotentTransClos_random :: Property
prop2_property_idempotentTransClos_random = forAll genSmallIntList prop2_property_idempotentTransClos

prop3_property_noDuplicatePairsTransClos_random :: Property
prop3_property_noDuplicatePairsTransClos_random = forAll genSmallIntList prop3_property_noDuplicatePairsTransClos

prop4_property_TrCloseSizeGreaterOrEqual_random :: Property
prop4_property_TrCloseSizeGreaterOrEqual_random = forAll genSmallIntList prop4_property_TrCloseSizeGreaterOrEqual

prop5_property_TrCloseContainsOriginal_random :: Property
prop5_property_TrCloseContainsOriginal_random = forAll genSmallIntList prop5_property_TrCloseContainsOriginal

intDomain :: [Rel Int]
intDomain = [[(1,2), (2,3), (3,4)]
            ,[(1,1)]
            ,[(1,2)]
            ,[(1,2), (2,1)]
            ,[]]
            
charDomain :: [Rel Char]
charDomain =
  [ [('a','b'), ('b','c')]
  , [('x','y'), ('y','x')]
  , [('a','a')]
  , []
  ]
  
stringDomain :: [Rel String]
stringDomain =
  [ [("Alice","Bob"), ("Bob","Carol")]
  , [("read","write"), ("write","test")]
  , [("start","end")]
  , []
  ]
  
boolDomain :: [Rel Bool]
boolDomain =
  [ [(False, True)]
  , [(False, True), (True, False)]
  , [(False, False)]
  , []
  ]

-- Generates an initial list raw of size n with elements between (10, 10), after de-deuplicating may contain less elements than n 
genSmallIntList :: Gen (Rel Int)
genSmallIntList = do
  n <- choose (0, 10)
  raw <- vectorOf n $ do
    x <- choose (-10, 10)
    y <- choose (-10, 10)
    return (x, y)
  return (sort (nub raw))