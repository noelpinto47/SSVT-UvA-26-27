module Exercise4 where

import Test.QuickCheck

type Rel a = [(a, a)]

-- A relation R is serial on domain A if every element in A has at least one outgoing pair.
-- isSerial :: Eq a => [a] -> Rel a -> Bool

k = 2
domain = [1,2,3]
rel = [(1,2), (2,3), (3,1)]

getOnlyPairWhichHasFirstElement = filter (\(x,y) -> x == k ) rel -- get pairs starting with x

checkIfThereExistsXinA = any(\(x,y) -> elem y domain) rel -- check if at least one has b in domain

-- To check if the relation is serial
-- 1. There exists a pair in R of the element in domain A
-- 2. Every element in relation R has a next
-- 3. The element in the pair has a y that is in domain A

checkOne :: Eq a => a -> [a] -> Rel a -> Bool
checkOne k domain rel = 
    let p = filter (\(x,y) -> x == k ) rel
    in any(\(x,y) -> elem y domain) p


isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial domain rel = all (\x -> checkOne x domain rel) domain


-- Eq a => - this is type constraint, it means a can be any type, but it must support equality checking (==) since the function has ==
-- [a] -- domain
-- Rel a - type alias for relation defined at the top of the file, can also be replaced by [(a,a)]
-- Bool - the output of the function

-- [(1,2), (2,3), (3,1)]  -- serial on [1,2,3]
-- [(1,1), (2,2), (3,3)]  -- identity relation
-- []                      -- empty relation

-- => - Type Constraint
-- -> - Function arrow


-- Properties:
-- 1. Is Identity relation serial?

prop_identityIsSerial :: [Int] -> Bool
prop_identityIsSerial domain = 
        let identityRel = [(x,x) | x <- domain]
        in isSerial domain identityRel


prop_identityIsSerialRel :: [Int] -> Rel Int -> Bool
prop_identityIsSerialRel domain rel = isSerial domain rel -- not true

-- 2. If a domain is not empty then it is not serial if the relation is empty

prop_emptyNotSerial :: Eq a => [a] -> Property
prop_emptyNotSerial domain = 
    not (null domain) ==> not (isSerial domain [])

-- 3. R is serial when x mod n is equal to y mod n for that both x and y should exist in domain A. 
-- I can test R is serial by taking x arbitrary and then assuming y = x, then always y will be in domain A as x is in domain A
-- and since y = x, x mod n will always be equal to y mod n when n > 0 
-- Proof by universal generalization (can also be achieved by proof by contradiction)

n = 3 -- a value 3 for testing without quickcheck

modRel :: Int -> [Int] -> Rel Int
modRel n domain = [ (x,y) | x <- domain, y <- domain, x `mod` n == y `mod` n]

prop_serialModRel :: [Int] -> Int -> Property
prop_serialModRel domain n = n > 0 ==> isSerial domain (modRel n domain)





