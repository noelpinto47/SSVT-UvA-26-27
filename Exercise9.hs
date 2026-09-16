
module Exercise9

where 

import Data.List
import System.Random
import Test.QuickCheck

genSmallNat :: Gen Integer
genSmallNat = chooseInteger (0, 5)

-- Functions from Lab0. We are allowed to use these
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..] 

oneTuple :: Int -> ([Integer], Integer)
oneTuple k = (take k primes, product (take k primes) + 1)

-- "| k <- [1..]" assigns sequential numbers, starting with 1 to the k variable
-- "snd" returns the second element of a 2 element tuple
counterexamples :: [([Integer], Integer)]
counterexamples = filter (\t -> not (prime (snd t))) [oneTuple k | k <- [1..]]

-- take 10 gives back a list
-- elements gives back a Generator which chooses one tuple of that list randomly
-- generate genRandomCounterTuple
genRandomCounterTuple :: Gen ([Integer], Integer)
genRandomCounterTuple = elements(take 5 counterexamples)

prop_SndIsNotPrime :: Property
-- forAll: Gen a -> (a -> prop) -> Property
prop_SndIsNotPrime = forAll genRandomCounterTuple (\t -> not(prime(snd t)))

test1 :: IO ()
test1 = quickCheckWith stdArgs { maxSuccess = 2 } prop_SndIsNotPrime

prop_SndIsNotPrimeWithoutGen :: Int -> Bool
prop_SndIsNotPrimeWithoutGen n = all (\t -> not(prime(snd t))) (take n counterexamples)

prop_FstIsPrime :: Property
prop_FstIsPrime = forAll genRandomCounterTuple (\t -> all prime (fst t))

test2 :: IO ()
test2 = quickCheckWith stdArgs { maxSuccess = 2 } prop_SndIsNotPrime
