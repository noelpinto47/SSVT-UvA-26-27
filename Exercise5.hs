import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

main :: IO ()
main = do

  let l1 = [0]::[Int]
  let l2 = [0]::[Int]
  let isDeran = isDerangement l1 l2
  putStrLn(show isDeran)
  quickCheck prop_2Implies1
  quickCheck prop_1Implies2
  quickCheck prop_3Implies2
  quickCheck prop_2Implies3
  quickCheck prop_3Implies1
  quickCheck prop_1Implies3

  putStrLn("property1LengthIsSame" ++ " " ++ show (property1LengthIsSame l1 l2))
  putStrLn("property2SortsToSameList" ++ " " ++ show (property2SortsToSameList l1 l2))
  putStrLn("property3AllIndicesDifferentValues" ++ " " ++ show (property3AllIndicesDifferentValues l1 l2))

  putStrLn("property1LengthIsSame" ++ " " ++ show (property1LengthIsSame l1 l2))
  putStrLn("property2SortsToSameList" ++ " " ++ show (property2SortsToSameList l1 l2))
  putStrLn("property3AllIndicesDifferentValues" ++ " " ++ show (property3AllIndicesDifferentValues l1 l2))

  let is3StrongerThan2 = stronger domain (uncurry property3AllIndicesDifferentValues) (uncurry property2SortsToSameList) 
  putStrLn("is3StrongerThan2 " ++ " " ++ show is3StrongerThan2)

  let is2StrongerThan1 = stronger domain (uncurry property2SortsToSameList) (uncurry property1LengthIsSame) 
  putStrLn("is2StrongerThan1 " ++ " " ++ show is2StrongerThan1)

  let is3StrongerThan1 = stronger domain (uncurry property3AllIndicesDifferentValues) (uncurry property1LengthIsSame) 
  putStrLn("is3StrongerThan1 " ++ " " ++ show is3StrongerThan1)
  
isDerangement :: Eq a => [a] -> [a] -> Bool 
isDerangement xs ys
  | not (elem ys (permutations xs)) = False
  | otherwise              = all id (zipWith (/=) xs ys)
  
deran :: Int -> [[Int]]
deran n =
  filter (isDerangement list) (permutations list)
  where
    list = [0 .. n - 1]

    
property1LengthIsSame::[Int] -> [Int] -> Bool
property1LengthIsSame xs ys = length xs == length ys

-- 2 is stronger than 1
property2SortsToSameList::[Int] -> [Int] -> Bool
property2SortsToSameList xs ys = sort xs == sort ys

-- 3 is not always stronger than 2(in case the lists are not permutations) or 1(in case lengths are different ie propert 1 fails but property 3 still succeeds)
property3AllIndicesDifferentValues::[Int] -> [Int] -> Bool
property3AllIndicesDifferentValues xs ys = all id (zipWith (/=) xs ys)

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\x -> p x --> q x) xs
weaker xs p q = stronger xs q p
  
-- Domain
domain::[([Int], [Int])]
domain = [([1,2,3], [2,3,1])
         ,([1,2,3], [1,3,2])
         ,([1,2,3], [1,2])
         ,([1,2,1], [2,1,1])
         ,([1,2,3], [4,5,6])
         ,([1,2,3], [4,5,6,7,8])
         ,([1,1,2,2], [2,2,1,1])
         ,([], [])
         ]

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

--Should pass
prop_2Implies1 :: [Int] -> [Int] -> Bool
prop_2Implies1 xs ys =
  property2SortsToSameList xs ys -->
  property1LengthIsSame xs ys
  
prop_1Implies2 :: [Int] -> [Int] -> Bool
prop_1Implies2 xs ys =
  property1LengthIsSame xs ys -->
  property2SortsToSameList xs ys
  
--should fail  
prop_3Implies2 :: [Int] -> [Int] -> Bool
prop_3Implies2 xs ys =
  property3AllIndicesDifferentValues xs ys -->
  property2SortsToSameList xs ys

prop_2Implies3 :: [Int] -> [Int] -> Bool
prop_2Implies3 xs ys =
  property2SortsToSameList xs ys -->
  property3AllIndicesDifferentValues xs ys

--should fail
prop_3Implies1 :: [Int] -> [Int] -> Bool
prop_3Implies1 xs ys =
  property3AllIndicesDifferentValues xs ys -->
  property1LengthIsSame xs ys
  
prop_1Implies3 :: [Int] -> [Int] -> Bool
prop_1Implies3 xs ys =
  property1LengthIsSame xs ys -->
  property3AllIndicesDifferentValues xs ys