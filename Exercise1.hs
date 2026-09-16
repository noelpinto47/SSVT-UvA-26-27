module Exercise1 where

import Test.QuickCheck

-- EXERCISE 1
-- TIME SPENT: 1 hour

-- the factorial function
-- base case: factorial 0 = 1
-- recursive case: n * factorial of (n-1)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

-- generator for small natural numbers
-- we keep the range small to avoid large numbers
generateNumber :: Gen Integer
generateNumber = choose (0, 20)

-- property 1: factorial of a natural number is always positive
-- factorial n > 0 for all n >= 0
prop_factorialPositive :: Property
prop_factorialPositive = forAll generateNumber $ \n -> factorial n > 0

-- property 2: factorial satisfies the recursive identity
-- test this by checking that factorial (n+1) == (n+1) * factorial n
-- we use n+1 to avoid the base case of 0
prop_factorialRecursiveIdentity :: Property
prop_factorialRecursiveIdentity = forAll generateNumber $ \n -> factorial (n+1) == (n+1) * factorial n

-- property 3: factorial is strictly increasing
-- factorial n < factorial (n + 1) for all n >= 0
prop_factorialIncreasing :: Property
prop_factorialIncreasing = forAll (choose (0, 19)) $ \n -> factorial n < factorial (n + 1)

-- main function to run the property tests
main :: IO ()
main = do
      quickCheck prop_factorialPositive
      quickCheck prop_factorialRecursiveIdentity
      quickCheck prop_factorialIncreasing