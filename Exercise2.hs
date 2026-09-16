module Exercise2

where 

import Data.List
import System.Random
import Test.QuickCheck

-- Has as input a list and gives back a list of lists
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = 
    let ps = powerset xs
    in ps ++ map (\sub -> x:sub) ps

genSmallNat :: Gen Integer
genSmallNat = chooseInteger (2, 10)

prop_TwoGreater :: Property
prop_TwoGreater = forAll genSmallNat (\n -> length (powerset [n .. n + 2]) == 2 * length (powerset [n .. n + 1]))

prop_LenPowerTwo :: Property
prop_LenPowerTwo = forAll genSmallNat (\n -> length (powerset[1 .. n]) == 2 ^ n)