module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck

-- Q × (L ∪ {τ}) × Q
cartesianProduct :: [State] -> [Label] -> [LabeledTransition]
cartesianProduct states labels =
    [ (from, label, to)
    | from  <- states
    , label <- labels ++ [tau] ++ [delta]
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

examples :: [(String, IOLTS)]
examples =
    [ ("counterImpl", counterImpl), ("counterModel", counterModel)
    , ("coffeeImpl1", coffeeImpl1), ("coffeeModel1", coffeeModel1)
    , ("coffeeImpl2", coffeeImpl2), ("coffeeModel2", coffeeModel2)
    , ("coffeeImpl3", coffeeImpl3), ("coffeeModel3", coffeeModel3)
    , ("coffeeImpl4", coffeeImpl4), ("coffeeModel4", coffeeModel4)
    , ("coffeeImpl5", coffeeImpl5), ("coffeeModel5", coffeeModel5)
    , ("coffeeImpl6", coffeeImpl6), ("coffeeModel6", coffeeModel6)
    , ("tretmanK1", tretmanK1), ("tretmanK2", tretmanK2), ("tretmanK3", tretmanK3)
    , ("tretmanI1", tretmanI1), ("tretmanI2", tretmanI2), ("tretmanI3", tretmanI3), ("tretmanI4", tretmanI4)
    , ("tretmanS1", tretmanS1), ("tretmanS2", tretmanS2), ("tretmanS3", tretmanS3), ("tretmanS4", tretmanS4)
    , ("tretmanR1", tretmanR1), ("tretmanR2", tretmanR2)
    ]

main :: IO ()
main = mapM_ putStrLn
    [ n ++ ": " ++ show (validateLTS lts)
    | (n, lts) <- examples
    ]