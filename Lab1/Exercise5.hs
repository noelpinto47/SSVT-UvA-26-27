module Exercise5 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
  
main :: IO ()
main = do
  putStrLn("Testing isDerangement on domain:")
  mapM_ (\(x, y) -> print (x, y, isDerangement x y)) domain
  putStrLn("QuickCheck tests on incomparable properties:")
  putStrLn("prop4_isDerangementSymmetric")
  quickCheck prop4_isDerangementSymmetric
  putStrLn("prop5_nonEmptyListCannotBeDerangementOfItself")
  quickCheck prop5_nonEmptyListCannotBeDerangementOfItself
  putStrLn("prop6_reversesPreserveDerangement")
  quickCheck prop6_reversesPreserveDerangement
  putStrLn("prop7_transformPreserveDerangement")
  quickCheck prop7_transformPreserveDerangement
  putStrLn("Testing comparable properties on domain for strength:")
  putStrLn("test_prop2Implies1:" ++ show test_prop2Implies1)
  putStrLn("test_prop1Implies2:" ++ show test_prop1Implies2)
  putStrLn("test_prop3Implies2:" ++ show test_prop3Implies2)
  putStrLn("test_prop2Implies3:" ++ show test_prop2Implies3)
  putStrLn("test_prop3Implies1:" ++ show test_prop3Implies1)
  putStrLn("test_prop1Implies3:" ++ show test_prop1Implies3)
  
  
isDerangement :: Eq a => [a] -> [a] -> Bool 
isDerangement xs ys
  | not (elem ys (permutations xs)) = False
  | otherwise              = all id (zipWith (/=) xs ys)
  
deran :: Int -> [[Int]]
deran n =
  filter (isDerangement list) (permutations list)
  where
    list = [0 .. n - 1]

-- generates a random list of integers of size n with elements between -10 and 10
genSmallIntList :: Gen [Int]
genSmallIntList = do
  n <- choose (1, 6)
  vectorOf n (choose (-10, 10))


-- Domain
domain::[([Int], [Int])]
domain = [([1,2,3], [2,3,1])
         ,([1,2,3], [1,3,2])
         ,([1,2,3], [1,2])
         ,([1,2,1], [2,1,1])
         ,([1,2,3], [4,5,6])
         ,([1,2,3], [4,5,6,7,8])
         ,([1,1,2,2], [2,2,1,1])
         ,([1,2,3], [4,5])
         ,([], [])
         ]

-- Comparable properties for lists of integers
prop1_LengthIsSame::[Int] -> [Int] -> Bool
prop1_LengthIsSame xs ys = length xs == length ys

-- 2 is stronger than 1
prop2_SortsToSameList::[Int] -> [Int] -> Bool
prop2_SortsToSameList xs ys = sort xs == sort ys

-- 3 is not always stronger than 2(in case the lists are not permutations) or 1(in case lengths are different ie propert 1 fails but property 3 still succeeds)
prop3_AllIndicesDifferentValues::[Int] -> [Int] -> Bool
prop3_AllIndicesDifferentValues xs ys = all id (zipWith (/=) xs ys)

--Testable properties for isDerangement function(Incomparable with each other in terms of logical implication as they test different aspects of the function)
-- Property 4 : isDerangement is symmetric
prop4_isDerangementSymmetric :: Property
prop4_isDerangementSymmetric = forAll genSmallIntList $ \xs -> forAll (elements (permutations xs)) $ \perm -> isDerangement xs perm == isDerangement perm xs

-- Property 5 : Any non-empty list cannot be a derangement of itself
prop5_nonEmptyListCannotBeDerangementOfItself :: Property
prop5_nonEmptyListCannotBeDerangementOfItself = forAll genSmallIntList $ \xs -> not (isDerangement xs xs)

-- Property 6 : reverses preserve derangement
prop6_reversesPreserveDerangement :: Property
prop6_reversesPreserveDerangement = forAll genSmallIntList $ \xs -> forAll (elements (permutations xs)) $ \perm -> isDerangement xs perm == isDerangement (reverse xs) (reverse perm)

-- Property 7 : transforming elements by adding 1preserves derangement
prop7_transformPreserveDerangement :: Property
prop7_transformPreserveDerangement = forAll genSmallIntList $ \xs -> forAll (elements (permutations xs)) $ \perm -> isDerangement xs perm == isDerangement (map (+1) xs) (map (+1) perm)



-- Test functions for comparing properties
--Should pass
test_prop2Implies1 :: Bool
test_prop2Implies1 =
  stronger domain
    (uncurry prop2_SortsToSameList)
    (uncurry prop1_LengthIsSame)
  
--Should fail: Counterexample ([1,2,3], [4,5,6])
test_prop1Implies2 :: Bool
test_prop1Implies2 =
  stronger domain
    (uncurry prop1_LengthIsSame)
    (uncurry prop2_SortsToSameList)
  
--should fail: Counterexample ([1,2,3], [4,5,6])
test_prop3Implies2 :: Bool
test_prop3Implies2 =
  stronger domain
    (uncurry prop3_AllIndicesDifferentValues)
    (uncurry prop2_SortsToSameList)

--should fail: Counterexample ([1,2,3], [1,2,3])
test_prop2Implies3 :: Bool
test_prop2Implies3 =
  stronger domain
    (uncurry prop2_SortsToSameList)
    (uncurry prop3_AllIndicesDifferentValues)

--should fail: Counterexample ([1,2,3], [4,5])
test_prop3Implies1 :: Bool
test_prop3Implies1 =
  stronger domain
    (uncurry prop3_AllIndicesDifferentValues)
    (uncurry prop1_LengthIsSame)

--should fail: Counterexample ([1,2,3], [1,2,3])
test_prop1Implies3 :: Bool
test_prop1Implies3 =
  stronger domain
    (uncurry prop1_LengthIsSame)
    (uncurry prop3_AllIndicesDifferentValues)


infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\x -> p x --> q x) xs
weaker xs p q = stronger xs q p

