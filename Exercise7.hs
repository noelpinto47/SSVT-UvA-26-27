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
instance Arbitrary Form where
    arbitrary = genForm 3

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
-- cap the size to avoid slow tests
main = do
    quickCheckWith stdArgs { maxSize = 3 } prop_cnfEquiv
