module Exercise2

where

import Data.List
import System.Random
import Test.QuickCheck

powerset_orderWrong :: [a] -> [[a]]
powerset_orderWrong [] = [[]]
powerset_orderWrong (x:xs) = 
    let ps = powerset_orderWrong xs
    in ps ++ map (\sub -> x:sub) ps

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset xs = 
    let ps = powerset (init xs)
        x  = last xs
    in ps ++ map (\sub -> sub ++ [x]) ps

genSmallNat :: Gen Integer
genSmallNat = chooseInteger (0, 10)

prop_TwoGreater :: Property
prop_TwoGreater = forAll genSmallNat (\n -> length (powerset [n .. n + 2]) == 2 * length (powerset [n .. n + 1]))

prop_General :: Integer -> Property
prop_General n = forAll genSmallNat (\p -> length (powerset [n .. n + p]) == 2 * length (powerset [n .. n + p - 1]))

prop_LenPowerTwo :: Integer -> Property
prop_LenPowerTwo n = forAll genSmallNat (\p -> length (powerset [n .. n + p]) == 2 ^ (p + 1))

prop_Distinct :: Integer -> Property
prop_Distinct n = forAll genSmallNat (\p -> length (powerset [n .. n + p]) == length (nub(powerset[n .. n + p])))

prop_containsEmpty :: Integer -> Property
prop_containsEmpty n = forAll genSmallNat (\p -> [] `elem` powerset [n .. n + p])

prop_containsInput :: Integer -> Property
prop_containsInput n = forAll genSmallNat (\p -> [n .. n + p] `elem` powerset [n .. n + p])
-----

prop_LenPowerTwoLimit :: Integer -> Property
prop_LenPowerTwoLimit n =
    n >= 1 && n <= 8 ==> length (powerset [1..n]) == 2 ^ n

main :: IO ()
main = do
    putStrLn("prop_TwoGreater:")
    quickCheck prop_TwoGreater

    putStrLn("prop_General:")
    quickCheck prop_General

    putStrLn("prop_LenPowerTwo:")
    quickCheck prop_LenPowerTwo

    putStrLn("prop_Distinct:")
    quickCheck prop_Distinct

    putStrLn("prop_containsEmpty:")
    quickCheck prop_containsEmpty

    putStrLn("prop_containsInput:")
    quickCheck prop_containsInput
