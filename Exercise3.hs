module Exercise3 where

import Test.QuickCheck
import Lecture3 ((-->))

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\x -> p x --> q x) xs
weaker xs p q = stronger xs q p

-- Domain specified by the exercise

domain :: [Int]
domain = [1 .. 10]

-- The four properties from Workshop 2 Exercise 3

prop1 :: Int -> Bool
prop1 x = even x && x > 3

prop2 :: Int -> Bool
prop2 x = even x || x > 3

prop3 :: Int -> Bool
prop3 x = (even x && x > 3) || even x

prop4 :: Int -> Bool
prop4 x = even x

-- Property 1 is stronger than Property 4
-- (even x && x > 3) -> even x

test1 :: Bool
test1 = stronger domain prop1 prop4


-- Property 4 is stronger than Property 2
-- even x -> (even x || x > 3)

test2 :: Bool
test2 = stronger domain prop4 prop2


-- Property 3 is at least as strong as Property 4
-- In fact, they are equivalent because:
-- (A && B) || A = A

test3a :: Bool
test3a = stronger domain prop3 prop4


-- Property 4 is at least as strong as Property 3

test3b :: Bool
test3b = stronger domain prop4 prop3


-- QuickCheck properties

prop_test1 :: Bool
prop_test1 = test1

prop_test2 :: Bool
prop_test2 = test2

prop_test3a :: Bool
prop_test3a = test3a

prop_test3b :: Bool
prop_test3b = test3b
