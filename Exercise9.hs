module Exercise9

where 

import Data.List
import System.Random
import Test.QuickCheck

-- Functions from Lab0. We are allowed to use these
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..]
---

-- Define the number of counterexamples to use for testing
maxCounterexamples :: Int
maxCounterexamples = 12

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
genRandomCounterTuple = elements(take maxCounterexamples counterexamples)

prop_SndIsNotPrime :: Property
-- forAll: Gen a -> (a -> prop) -> Property
prop_SndIsNotPrime = forAll genRandomCounterTuple (\t -> not(prime(snd t)))

prop_SndIsNotPrimeWithoutGen :: Int -> Bool
prop_SndIsNotPrimeWithoutGen n = all (\t -> not(prime(snd t))) (take n counterexamples)

prop_FstIsPrimeWithoutGen :: Int -> Bool
prop_FstIsPrimeWithoutGen n = all (\t -> all prime (fst t)) (take n counterexamples)

prop_FstOrderPrimeWithoutGen :: Int -> Bool
prop_FstOrderPrimeWithoutGen n = all (\t -> fst t == take (length (fst t)) primes) (take n counterexamples)

prop_SecondCalcFirstWithoutGen :: Int -> Bool
prop_SecondCalcFirstWithoutGen n = all (\t -> product (fst t) + 1 == snd t) (take n counterexamples)


main :: IO ()
main = do
    putStrLn("prop_SndIsNotPrime:")
    quickCheckWith stdArgs { maxSuccess = 5 } prop_SndIsNotPrime

    putStrLn("prop_SndIsNotPrimeWithoutGen:")
    putStrLn $ show (prop_SndIsNotPrimeWithoutGen maxCounterexamples)

    putStrLn("prop_FstIsPrimeWithoutGen:")
    putStrLn $ show (prop_FstIsPrimeWithoutGen maxCounterexamples)

    putStrLn("prop_FstOrderPrimeWithoutGen:")
    putStrLn $ show (prop_FstOrderPrimeWithoutGen maxCounterexamples)

    putStrLn("prop_SecondCalcFirstWithoutGen:")
    putStrLn $ show (prop_SecondCalcFirstWithoutGen maxCounterexamples)

    
