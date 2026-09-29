module Exercise6 where

import Data.List
import System.Random
import Test.QuickCheck

import Exercise3 (symClos)
import Exercise5 (trClos, Rel)

main :: IO ()
main = do
    putStrLn("Testing symClos on Int domain([Input], [Symmetric Closure Output]):")
    mapM_ (\r -> print (r, symClos r)) intSymDomain
    putStrLn("Testing symClos on Char domain([Input], [Symmetric Closure Output]):")
    mapM_ (\r -> print (r, symClos r)) charSymDomain
    putStrLn("Testing symClos on String domain([Input], [Symmetric Closure Output]):")
    mapM_ (\r -> print (r, symClos r)) stringSymDomain
    putStrLn("Testing symClos on Bool domain([Input], [Symmetric Closure Output]):")
    mapM_ (\r -> print (r, symClos r)) boolSymDomain
    
    putStrLn("Testing trClos on Int domain([Input], [Transitive Closure Output]):")
    mapM_ (\r -> print (r, trClos r)) intTrDomain
    putStrLn("Testing trClos on Char domain([Input], [Transitive Closure Output]):")
    mapM_ (\r -> print (r, trClos r)) charTrDomain
    putStrLn("Testing trClos on String domain([Input], [Transitive Closure Output]):")
    mapM_ (\r -> print (r, trClos r)) stringTrDomain
    putStrLn("Testing trClos on Bool domain([Input], [Transitive Closure Output]):")
    mapM_ (\r -> print (r, trClos r)) boolTrDomain
    

    putStrLn("QuickCheck tests on symClos properties(excluding prop1_SymClose_SizeIsGreaterOrEqual)")
    quickCheck prop2_SymClose_symmetricElementShouldExist
    quickCheck prop3_SymClose_originalPairsRemain
    quickCheck prop4_SymClose_idempotent
    quickCheck prop5_SymClose_noDuplicatePairs

    putStrLn("QuickCheck tests using custom generator genSmallIntList on symClos properties:")
    quickCheck prop1_SymClose_SizeIsGreaterOrEqual_random
    quickCheck prop2_SymClose_symmetricElementShouldExist_random
    quickCheck prop3_SymClose_originalPairsRemain_random
    quickCheck prop4_SymClose_idempotent_random
    quickCheck prop5_SymClose_noDuplicatePairs_random

    putStrLn("QuickCheck tests on trClose properties(excluding prop3_property_noDuplicatePairsTransClos):")
    quickCheck prop1_trClose_transitiveElementShouldExist
    quickCheck prop2_trClose_idempotent
    quickCheck prop5_trClose_ContainsOriginal

    putStrLn("QuickCheck tests on trClos properties with random sets:")
    quickCheck prop1_trClose_transitiveElementShouldExist_random
    quickCheck prop2_trClose_idempotent_random
    quickCheck prop3_trClose_noDuplicatePairs_random
    quickCheck prop4_trClose_SizeGreaterOrEqual_random
    quickCheck prop5_trClose_ContainsOriginal_random


-- --QuickCheck Properties of the symmetric closure function
-- This is run only on generator because QuickCheck might create duplicate elements in input.
-- Rest of the properties can be run using quickcheck input generation.
prop1_SymClose_SizeIsGreaterOrEqual :: Rel Int -> Bool
prop1_SymClose_SizeIsGreaterOrEqual r = length (symClos r) >= (length r)

--Every pair in the symmetric closure has its symmetric counterpart.
prop2_SymClose_symmetricElementShouldExist :: Rel Int -> Bool
prop2_SymClose_symmetricElementShouldExist r = all (\(x,y) -> (y,x) `elem` symClos r) r

--Every pair in the original relation remains in the result.
prop3_SymClose_originalPairsRemain :: Rel Int -> Bool
prop3_SymClose_originalPairsRemain r = all (`elem` symClos r) r

--Applying symmetric closure twice changes nothing further:
prop4_SymClose_idempotent :: Rel Int -> Bool
prop4_SymClose_idempotent r = symClos (symClos r) == symClos r

--The result should not contain duplicate pairs.
prop5_SymClose_noDuplicatePairs :: Rel Int -> Bool
prop5_SymClose_noDuplicatePairs r = length (symClos r) == length (nub (symClos r))

--Same properties with randomly generated sets of integers - these lists are ordered(sorted) and do NOT contain duplicates
prop1_SymClose_SizeIsGreaterOrEqual_random :: Property
prop1_SymClose_SizeIsGreaterOrEqual_random = forAll genSmallIntList prop1_SymClose_SizeIsGreaterOrEqual

prop2_SymClose_symmetricElementShouldExist_random :: Property
prop2_SymClose_symmetricElementShouldExist_random = forAll genSmallIntList prop2_SymClose_symmetricElementShouldExist

prop3_SymClose_originalPairsRemain_random :: Property
prop3_SymClose_originalPairsRemain_random = forAll genSmallIntList prop3_SymClose_originalPairsRemain

prop4_SymClose_idempotent_random :: Property
prop4_SymClose_idempotent_random = forAll genSmallIntList prop4_SymClose_idempotent

prop5_SymClose_noDuplicatePairs_random :: Property
prop5_SymClose_noDuplicatePairs_random = forAll genSmallIntList prop5_SymClose_noDuplicatePairs



-- QuickCheck properties for transitive closure
--Every pair in the transitive closure should have its transitive counterpart.
prop1_trClose_transitiveElementShouldExist :: Rel Int -> Bool
prop1_trClose_transitiveElementShouldExist r = all (\(x,y) -> all (\(y',z) -> y /= y' || (x,z) `elem` trClos r) r) (trClos r)

--Applying transitive closure twice changes nothing further:
prop2_trClose_idempotent :: Rel Int -> Bool
prop2_trClose_idempotent r = trClos (trClos r) == trClos r

--The result should not contain duplicate pairs.
prop3_trClose_noDuplicatePairs :: Rel Int -> Bool
prop3_trClose_noDuplicatePairs r = length (trClos r) == length (nub (trClos r))

prop4_trClose_SizeGreaterOrEqual :: Rel Int -> Bool
prop4_trClose_SizeGreaterOrEqual r = length (trClos r) >= length r

prop5_trClose_ContainsOriginal :: Rel Int -> Bool
prop5_trClose_ContainsOriginal r = all (`elem` trClos r) r

--Same properties with randomly generated sets of integers
prop1_trClose_transitiveElementShouldExist_random :: Property
prop1_trClose_transitiveElementShouldExist_random = forAll genSmallIntList prop1_trClose_transitiveElementShouldExist

prop2_trClose_idempotent_random :: Property
prop2_trClose_idempotent_random = forAll genSmallIntList prop2_trClose_idempotent

prop3_trClose_noDuplicatePairs_random :: Property
prop3_trClose_noDuplicatePairs_random = forAll genSmallIntList prop3_trClose_noDuplicatePairs

prop4_trClose_SizeGreaterOrEqual_random :: Property
prop4_trClose_SizeGreaterOrEqual_random = forAll genSmallIntList prop4_trClose_SizeGreaterOrEqual

prop5_trClose_ContainsOriginal_random :: Property
prop5_trClose_ContainsOriginal_random = forAll genSmallIntList prop5_trClose_ContainsOriginal

intSymDomain :: [Rel Int]
intSymDomain = [[(1,2), (2,3), (3,4)]
            ,[(1,1)]
            ,[(1,2)]
            ,[(1,2), (2,1)]
            ,[]]
            
charSymDomain :: [Rel Char]
charSymDomain =
  [ [('a','b'), ('b','c')]
  , [('x','y'), ('y','x')]
  , [('a','a')]
  , []
  ]
  
stringSymDomain :: [Rel String]
stringSymDomain =
  [ [("Alice","Bob"), ("Bob","Carol")]
  , [("read","write"), ("write","test")]
  , [("start","end")]
  , []
  ]
  
boolSymDomain :: [Rel Bool]
boolSymDomain =
  [ [(False, True)]
  , [(False, True), (True, False)]
  , [(False, False)]
  , []
  ]
  
-- Transitive-closure domains:
-- focus on chains, cycles, disconnected components, and branching paths.

intTrDomain :: [Rel Int]
intTrDomain =
  [ []
  , [(1,1)]
  , [(1,2)]
  , [(1,2), (2,3)]
  , [(1,2), (2,3), (3,4)]
  , [(1,2), (2,3), (3,1)]
  , [(1,2), (3,4)]
  , [(1,2), (1,3), (2,4), (3,4)]
  ]

charTrDomain :: [Rel Char]
charTrDomain =
  [ []
  , [('a','a')]
  , [('a','b'), ('b','c')]
  , [('a','b'), ('b','c'), ('c','d')]
  , [('a','b'), ('b','c'), ('c','a')]
  , [('a','b'), ('d','e')]
  , [('a','b'), ('a','c'), ('b','d'), ('c','d')]
  ]

stringTrDomain :: [Rel String]
stringTrDomain =
  [ []
  , [("start","start")]
  , [("Alice","Bob"), ("Bob","Carol")]
  , [("Alice","Bob"), ("Bob","Carol"), ("Carol","Dave")]
  , [("A","B"), ("B","C"), ("C","A")]
  , [("read","write"), ("test","deploy")]
  , [("design","implement"), ("design","test"),
     ("implement","deploy"), ("test","deploy")]
  ]

boolTrDomain :: [Rel Bool]
boolTrDomain =
  [ []
  , [(False,False)]
  , [(False,True)]
  , [(False,True), (True,False)]
  , [(False,False), (False,True)]
  , [(False,True), (True,True)]
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