module Exercise4 where

import LTS
import Test.QuickCheck
import Data.List (nub)

-- Follow a single observable label from a set of states
stepWith :: [LabeledTransition] -> [State] -> Label -> [State]
stepWith lt qs l =
  makeSet [ s' | q <- qs, (s', lab) <- nextTransitions' lt q, lab == l ]

-- ε-closure: follow tau transitions until fixpoint
epsClose :: [LabeledTransition] -> [State] -> [State]
epsClose lt qs = go (makeSet qs)
  where
    go visited =
      let visited' = makeSet (visited ++ stepWith lt visited tau)
      in if visited' == visited then visited else go visited'

-- q is quiescent iff it has no outgoing output and no outgoing tau
isQuiescent :: IOLTS -> State -> Bool
isQuiescent (_, _, outs, lt, _) q =
  null [ l | (_, l) <- nextTransitions' lt q, l == tau || elem l outs ]

-- after function
after :: IOLTS -> Trace -> [State]
after iolts@(_, _, _, lt, q0) trace = go (epsClose lt [q0]) trace
  where
    go qs []       = qs
    go qs (l : ls)
      | l == delta = go (filter (isQuiescent iolts) qs) ls
      | otherwise  = go (epsClose lt (stepWith lt qs l)) ls

-- All sequences of a given length
sequences :: [a] -> Int -> [[a]]
sequences _  0 = [[]]
sequences xs n = [ x : rest | x <- xs, rest <- sequences xs (n - 1) ]

-- straces: ALL suspension traces (infinite, lazy), dead traces dropped.
straces :: IOLTS -> [Trace]
straces iolts@(_, ins, outs, _, _) =
  [ sigma
  | sigma <- concatMap (sequences (nub (ins ++ outs ++ [delta]))) [0 ..]
  , not (null (after iolts sigma)) ]

-- Unit tests
runTests :: IO ()
runTests = do
  putStrLn "== Regular traces =="
  print (after tretmanK1 []                    == [0])
  print (after tretmanK1 ["but"]               == [1])
  print (after tretmanK1 ["but","liq"]         == [2])
  print (after tretmanK1 ["but","but"]         == [1])
  print (after coffeeImpl6 []                  == [1])
  print (after coffeeImpl6 ["coin"]            == [2,3])
  print (after coffeeImpl6 ["coin","coffee"]   == [4])
  print (after coffeeModel6 ["coin"]           == [2,3])   -- τ absorbed

  putStrLn "== Suspension traces (δ) =="
  print (after tretmanK1 [delta]               == [0])     -- 0 quiescent
  print (after tretmanK1 ["but", delta]        == [])      -- 1 has !liq
  print (after tretmanK1 ["but","liq",delta]   == [2])     -- 2 quiescent
  print (after tretmanK1 [delta, "but"]        == [1])
  print (after coffeeModel6 [delta]            == [1])     -- 1 quiescent
  print (after coffeeModel6 ["coin", delta]    == [3])     -- τ→3, 3 quiescent

-- QuickCheck: random traces over tretmanK1's real alphabet plus delta
genLabel :: Gen Label
genLabel =
  let (_, ins, outs, _, _) = tretmanK1
  in elements (nub (ins ++ outs ++ [delta]))

genTrace :: Gen Trace
genTrace = do
  n <- choose (0, 4)
  vectorOf n genLabel

-- 1. Empty trace = ε-closure of q0 (sanity check on the base case)
prop_empty :: Bool
prop_empty =
  let (_,_,_,lt,q0) = tretmanK1
  in after tretmanK1 [] == epsClose lt [q0]

-- 2. δ only shrinks the after-set: after(σ·δ) ⊆ after(σ)
prop_delta :: Property
prop_delta = forAll genTrace $ \sigma ->
  let qs  = after tretmanK1 sigma
      qs' = after tretmanK1 (sigma ++ [delta])
  in all (`elem` qs) qs'

-- 3. Every state surviving a δ step is quiescent
prop_delta_quiescent :: Property
prop_delta_quiescent = forAll genTrace $ \sigma ->
  all (isQuiescent tretmanK1) (after tretmanK1 (sigma ++ [delta]))

main :: IO ()
main = do
  runTests
  putStrLn "== QuickCheck =="
  quickCheck prop_empty
  quickCheck prop_delta
  quickCheck prop_delta_quiescent
