module Exercise2 where

import Data.List
import LTS
import Test.QuickCheck
import Exercise1

genNumStates :: Gen Int
genNumStates = choose (1, 5)

genStates :: Gen [State]
genStates = do
    n <- genNumStates
    take n <$> shuffle [1..10]

inputLabelsPool :: [Label]
inputLabelsPool =
  [ "?insertCoin"
  , "?insertCard"
  , "?cancel"
  , "?selectEspresso"
  , "?selectCoffee"
  , "?selectTea"
  , "?selectHotChocolate"
  , "?selectWater"
  , "?selectDecaf"
  , "?requestRefund"
  ]

genNumInputLabels :: Gen Int
genNumInputLabels = choose (1, 5)

genInputLabels :: Gen [Label]
genInputLabels = do
    n <- genNumInputLabels
    take n <$> shuffle inputLabelsPool

outputLabelsPool :: [Label]
outputLabelsPool =
  [ "!dispenseEspresso"
  , "!dispenseCoffee"
  , "!dispenseTea"
  , "!dispenseHotChocolate"
  , "!dispenseWater"
  , "!returnChange"
  , "!refundIssued"
  , "!outOfStock"
  , "!paymentRejected"
  , "!machineReady"
  ]

genNumOutputLabels :: Gen Int
genNumOutputLabels = choose (1, 5)

genOutputLabels :: Gen [Label]
genOutputLabels = do
    n <- genNumOutputLabels
    take n <$> shuffle outputLabelsPool

inputTransition :: [State] -> [Label] -> Gen LabeledTransition
inputTransition ss inputLabels = do
    start <- elements ss
    input <- elements inputLabels
    end <- elements ss
    return (start, input, end)

outputTransition :: [State] -> [Label] -> Gen LabeledTransition
outputTransition ss outputLabels = do
    start <- elements ss
    output <- elements outputLabels
    end <- elements ss
    return (start, output, end)

-- Tau self loop is allowed
tauTransition :: [State] -> Gen LabeledTransition
tauTransition ss = do
    start <- elements ss
    end <- elements ss
    return (start, tau, end)

numTransitions :: Gen Int
numTransitions = choose (1, 10)

genTransitions :: [State] -> [Label] -> [Label] -> Gen [LabeledTransition]
genTransitions ss inputLabels outputLabels = do
    n <- numTransitions
    nub <$> vectorOf n
        (oneof
            [ inputTransition ss inputLabels
            , outputTransition ss outputLabels
            , tauTransition ss
            ])

genValidIOLTS :: Gen IOLTS
genValidIOLTS = do
    ss <- genStates
    initialState <- elements ss
    inputLabels <- genInputLabels
    outputLabels <- genOutputLabels
    transitions <- genTransitions ss inputLabels outputLabels

    return (ss, inputLabels, outputLabels, transitions, initialState)

genEmptyStatesIOLTS :: Gen IOLTS
genEmptyStatesIOLTS = do
    inputLabels <- genInputLabels
    outputLabels <- genOutputLabels
    return ([], inputLabels, outputLabels, [], 1)

genTauInInputLabelsIOLTS :: Gen IOLTS
genTauInInputLabelsIOLTS = do
    ss <- genStates
    initialState <- elements ss
    inputLabels <- genInputLabels
    outputLabels <- genOutputLabels
    let inputLabels_false = inputLabels ++ [tau]
    transitions <- genTransitions ss inputLabels_false outputLabels
    return (ss, inputLabels_false, outputLabels, transitions, initialState)

genInitialStateNotInStatesIOLTS :: Gen IOLTS
genInitialStateNotInStatesIOLTS = do
    ss <- genStates
    inputLabels <- genInputLabels
    outputLabels <- genOutputLabels
    transitions <- genTransitions ss inputLabels outputLabels
    let initialState = 11
    return (ss, inputLabels, outputLabels, transitions, initialState)

genOverlappingLabelsIOLTS :: Gen IOLTS
genOverlappingLabelsIOLTS = do
    ss <- genStates
    initialState <- elements ss
    inputLabels <- genInputLabels
    outputLabels <- genOutputLabels
    -- If input labels would be empty, the head function would be a problem.
    -- But genNumInputLabels creates minimum 1 element, so it is fine. 
    let sharedLabel = head inputLabels
    let outputLabels_false = sharedLabel : outputLabels
    transitions <- genTransitions ss inputLabels outputLabels_false
    return (ss, inputLabels, outputLabels_false, transitions, initialState)

genInvalidTransitionIOLTS :: Gen IOLTS
genInvalidTransitionIOLTS = do
    ss <- genStates
    initialState <- elements ss
    inputLabels <- genInputLabels
    outputLabels <- genOutputLabels
    transitions <- genTransitions ss inputLabels outputLabels
    start <- elements ss
    end <- elements ss
    let invalidTransition = (start, "falseLabel", end)
    let transitions_false = invalidTransition : transitions
    return (ss, inputLabels, outputLabels, transitions_false, initialState)

main :: IO ()
main = do

    -- Valid IOLTS generator
    quickCheck (forAll genValidIOLTS Exercise1.prop_statesNotEmpty)
    quickCheck (forAll genValidIOLTS Exercise1.prop_initialStateElementOf)
    quickCheck (forAll genValidIOLTS Exercise1.prop_tauNotInInputLabels)
    quickCheck (forAll genValidIOLTS Exercise1.prop_intersectionIsEmpty)
    quickCheck (forAll genValidIOLTS Exercise1.prop_transitionsSubsetOfCartesianProduct)
    quickCheck (forAll genValidIOLTS Exercise1.validateLTS)

    -- Invalid generators
    -- expectFailure is used to demonstrate the failures and nevertheless count it as OK.
    quickCheck (expectFailure (forAll genEmptyStatesIOLTS Exercise1.prop_statesNotEmpty))
    quickCheck (expectFailure (forAll genTauInInputLabelsIOLTS Exercise1.prop_tauNotInInputLabels))
    quickCheck (expectFailure (forAll genInitialStateNotInStatesIOLTS Exercise1.prop_initialStateElementOf))
    quickCheck (expectFailure (forAll genOverlappingLabelsIOLTS Exercise1.prop_intersectionIsEmpty))
    quickCheck (expectFailure (forAll genInvalidTransitionIOLTS Exercise1.prop_transitionsSubsetOfCartesianProduct))