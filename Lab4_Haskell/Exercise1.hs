module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck

-- Q × (L ∪ {τ}) × Q
cartesianProduct :: [State] -> [Label] -> [LabeledTransition]
cartesianProduct states labels =
    [ (from, label, to)
    | from  <- states
    , label <- labels ++ [tau]
    , to    <- states
    ]

validateLTS :: IOLTS -> Bool
validateLTS ([], _, _, _, _) = False
validateLTS (states, input_labels, output_labels, transitions, initial_state)
    | initial_state `notElem` states = False
    | tau `elem` input_labels = False
    | not (null (input_labels `intersect` output_labels)) = False
    | not allTransitionsValid = False
    | otherwise = True
    where
        allowedTransitions = cartesianProduct states (input_labels ++ output_labels)

        allTransitionsValid = all (\transition -> transition `elem` allowedTransitions) transitions

prop_statesNotEmpty :: IOLTS -> Bool
prop_statesNotEmpty (states, _,_,_,_) = not (null states)

prop_initialStateElementOf :: IOLTS -> Bool
prop_initialStateElementOf (states, _,_,_,initial_state) = initial_state `elem` states

prop_tauNotInInputLabels :: IOLTS -> Bool
prop_tauNotInInputLabels (_, input_labels,_,_,_) = tau `notElem` input_labels

prop_intersectionIsEmpty :: IOLTS -> Bool
prop_intersectionIsEmpty (_, input_labels, output_labels,_,_) = null (input_labels `intersect` output_labels)

prop_transitionsSubsetOfCartesianProduct :: IOLTS -> Bool
prop_transitionsSubsetOfCartesianProduct (states, input_labels, output_labels, transitions, _) =
        all (\transition -> transition `elem` allowedTransitions) transitions
    where
        allowedTransitions = cartesianProduct states (input_labels ++ output_labels)

-- All predefined IOLTS examples from LTS.hs
-- (the plain LTS examples like tretmanP..V have no ?/! labels and are 4-tuples,
--  so they cannot be tested with the IOLTS properties)
examples :: [IOLTS]
examples =
    [ counterImpl, counterModel
    , coffeeImpl1, coffeeModel1
    , coffeeImpl2, coffeeModel2
    , coffeeImpl3, coffeeModel3
    , coffeeImpl4, coffeeModel4
    , coffeeImpl5, coffeeModel5
    , coffeeImpl6, coffeeModel6
    , tretmanK1, tretmanK2, tretmanK3
    , tretmanI1, tretmanI2, tretmanI3, tretmanI4
    , tretmanS1, tretmanS2, tretmanS3, tretmanS4
    , tretmanR1, tretmanR2
    ]

main :: IO ()
main = do
    let ltsImplementations = examples
    print (all validateLTS ltsImplementations) -- if all LTS implementations confirm the validateLTS function, it returns true