module Exercise7 where

-- TAKES ANY LOGICAL FORMULA AND CONVERTS IT TO CNF
-- CNF is formular like (p OR q) AND (NOT p OR r) AND (q OR NOT r)
-- three steps: remove arrows, move NOTs inwards, fix AND/OR structure

import Lecture3
import Test.QuickCheck

-- EXERCISE 7
-- TIME SPENT: 4 hours

-- step 3: fix the AND/OR structure
-- e.g. r OR (p AND q)  becomes (r OR p) AND (r OR q)


distribute :: Form -> Form
-- if there is an OR
distribute (Dsj subformulas) =
    -- fix everything inside the OR first
    let distributedSubFormulas = map distribute subformulas
    -- check if fixed parts have an AND
    in case break isCnj distributedSubFormulas of
        -- AND inside OR, so distribute the OR over the AND
        (otherFormulas, Cnj cnjs : remainingFormulas) ->
            distribute (Cnj (map (\conjunct -> Dsj (otherFormulas ++ [conjunct] ++ remainingFormulas)) cnjs))
        -- no AND inside OR, so do nothing
        _ -> Dsj distributedSubFormulas


-- if have an AND, just fix everything inside the AND
distribute (Cnj subformulas) = Cnj (map distribute subformulas)

-- anything else (variable, NOT variable) is already in CNF, so leave as is
distribute formula = formula

-- check if a formula is an AND
isCnj :: Form -> Bool
isCnj (Cnj _) = True
isCnj _ = False

-- full three-steps CNF conversion: remove arrows, move NOTs inwards, fix AND/OR structure
cnf :: Form -> Form
cnf = distribute . nnf . arrowfree

-- testing the cnf function by checking that the original formula and the CNF version give the same true/false answer
prop_cnfEquiv :: Form -> Bool
prop_cnfEquiv formula = all (\valuation -> evl valuation formula == evl valuation (cnf formula)) (genVals (propNames formula))

-- tells QuickCheck how to generate random formulas for testing
-- cap the size to avoid slow tests
instance Arbitrary Form where
    arbitrary = sized (\n -> genForm (min n 3))

-- generate a random formula of a given size
genForm :: Int -> Gen Form
genForm 0 = Prop <$> choose (1, 3)
genForm depth = oneof
                    [ Prop <$> choose (1, 3) -- single variable
                    , Neg <$> genForm (depth-1) -- NOT variable
                    , Cnj <$> vectorOf 2 (genForm (depth `div` 2)) -- AND of two formulas
                    , Dsj <$> vectorOf 2 (genForm (depth `div` 2)) -- OR of two formulas
                    , Impl <$> genForm (depth `div` 2) <*> genForm (depth `div` 2) -- formula ==> formula
                    , Equiv <$> genForm (depth `div` 2) <*> genForm (depth `div` 2)
                    ] -- formula <==> formula

-- main function run the property test
main :: IO ()
main = quickCheck prop_cnfEquiv


{- 


CNF NOTES

(a OR b) AND (NOT a OR c) AND (b OR NOT c)   -   is in CNF
- because the whole thing is in ANDs
- inside each AND, there are only ORs
- inside each OR, you can only have variables or NOTs of variables

p OR (q AND r)   -   is not in CNF
- because there is an AND inside an OR

in maths
a(b+c) = ab + ac
a OR (b AND c) = (a OR b) AND (a OR c)

OR is like x
AND is like +
NOT is like -

also
p ==> q becomes NOT p OR q
p <=> q becomes (p AND q) OR (NOT p AND NOT q)


CNJ is AND
DSJ is OR
NEG is NOT

-}