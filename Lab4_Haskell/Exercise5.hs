module Exercise5 where

import Data.List
import LTS
import Test.QuickCheck
import Control.Exception
import System.IO.Unsafe

{-
Exercise 5
-}

-- EXERCISE 5
-- Time spent: 3 hours
-- Dependencies: base (Control.Exception, System.IO.Unsafe)

-- door takes its current state and an input and returns the new state and an output
type Door = State -> Label -> (State, Label)

-- specification of the door, labels keep ? and ! so inputs and outputs can be told apart
-- 0 = open, 2 = closed and unlocked, 5 = closed and locked
doorTransitions :: [LabeledTransition]
doorTransitions = [(0, "?close", 1), (1, "!closed", 2), (2, "?open", 3), (3, "!opened", 0),
                   (2, "?lock", 4), (4, "!locked", 5), (5, "?unlock", 6), (6, "!unlocked", 2)]

doorModel :: IOLTS
doorModel = createIOLTS doorTransitions

doorLTS :: LTS
doorLTS = createLTS doorTransitions


-- states reached from a set of states with a label
afterLabel :: [LabeledTransition] -> [State] -> Label -> [State]
afterLabel ts ss l = makeSet [t | (f, l', t) <- ts, f `elem` ss, l' == l]

-- labels enabled in a set of states that start with the given prefix (? or !)
enabled :: [LabeledTransition] -> [State] -> Char -> [Label]
enabled ts ss c = makeSet [l | (f, l, _) <- ts, f `elem` ss, take 1 l == [c]]

-- runs the door and catches its erors so they can be reported as a failure
safeStep :: Door -> State -> Label -> Either String (State, Label)
safeStep sut s i = unsafePerformIO $ do
    r <- try (evaluate (forceResult (sut s i)))
    return $ case r of
        Left (ErrorCall msg) -> Left msg
        Right result -> Right result
  where
    forceResult (s', o) = s' `seq` length o `seq` (s', o)
{-# NOINLINE safeStep #-}

-- explores every reachable (model states, door state) pair once
-- only inputs the model allows are sent, and every output must be allowed by the model (ioco)
checkSUT :: LTS -> Door -> Either String ()
checkSUT (_, _, ts, q0) sut = explore [([q0], 0, [])] []
  where
    explore [] _ = Right ()
    explore ((modelStates, doorState, trace) : queue) seen
        | (modelStates, doorState) `elem` seen = explore queue seen
        | otherwise = case mapM (tryInput modelStates doorState trace) (enabled ts modelStates '?') of
            Left msg -> Left msg
            Right next -> explore (queue ++ next) ((modelStates, doorState) : seen)

    -- sends one input to the door and checks its output against the model
    tryInput modelStates doorState trace input =
        let afterInput = afterLabel ts modelStates input
            allowedOutputs = map (drop 1) (enabled ts afterInput '!')
            newTrace = trace ++ [input]
            failWith problem = Left (failMsg newTrace problem input allowedOutputs)
        in case safeStep sut doorState (drop 1 input) of
            Left msg -> failWith ("crashed with " ++ show msg)
            Right (newDoorState, output) ->
                if output `elem` allowedOutputs
                    then Right (afterLabel ts afterInput ('!' : output), newDoorState, newTrace ++ ['!' : output])
                    else failWith ("responded " ++ show output)


-- short description of the flaw and how to fix it
failMsg :: Trace -> String -> Label -> [Label] -> String
failMsg trace problem input allowedOutputs = "after " ++ show trace ++ " the door " ++ problem
    ++ ". Fix: " ++ show (drop 1 input) ++ " should respond " ++ intercalate " or " (map show allowedOutputs)

-- returns True if the door implements the model, otherwise throws the error message
testLTSAgainstSUT :: LTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT model sut = case checkSUT model sut of
    Left msg -> error msg
    Right () -> True



-- main function to run the test on every door and print the result
main :: IO ()
main = do
    let doors = [doorImpl1, doorImpl2, doorImpl3, doorImpl4, doorImpl5, doorImpl6, doorImpl7, doorImpl8]
    mapM_ testDoor (zip [1 :: Int ..] doors)
  where
    testDoor (n, door) = case checkSUT doorLTS door of
        Left msg -> putStrLn $ "doorImpl" ++ show n ++ ": " ++ msg
        Right () -> putStrLn $ "doorImpl" ++ show n ++ ": PASS"
