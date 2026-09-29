module Exercise5 where

import Data.List (nub, union, sort)

type Rel a = [(a, a)]
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- trClos :: Ord a => Rel a -> Rel a

-- Transitive closure of R: the smallest transitive relation that contains R.
--   (1) R is included in it
--   (2) transitive: (x,y) and (y,z) in it => (x,z) in it
-- Idea: R @@ R are the paths of length 2, R @@ R @@ R of length 3, ... so keep adding one more step
-- until nothing new appears (a fixed point).

-- one round: what we have + everything reachable by one more R-step
step :: Ord a => Rel a -> Rel a -> Rel a
step r current = current `union` (current @@ r)


trClos :: Ord a => Rel a -> Rel a
trClos r = sort result
  where
    r' = nub r   -- to avoid duplicates in the result
    result = loop r' r'       -- keep applying rounds until nothing new appears
    loop r current = 
        let next = step r current
        in if sort next == sort current
           then next
           else loop r next


-- trClos r = loop r r — start the loop with current = r (the original relation is always part of its own closure)
-- let next = current union (current @@ r) — compute one round: existing pairs + newly reachable pairs
-- if sort next == sort current — if nothing changed (after sorting to ignore order), we're done
-- then next — return the fixed point
-- else loop r next — otherwise, continue with next as the new current

-- Simons function for a fancy banner in the exercise string
exercise :: Integer -> String -> String
exercise x name
  | x == 1 = banner
  | otherwise = "\n" ++ banner
  where
    contents = "== Exercise " ++ show x ++ ": " ++ name ++ " =="
    delimiter = replicate (length contents) '='
    banner = delimiter ++ "\n" ++ contents ++ "\n" ++ delimiter

--- MAIN
main :: IO ()
main = do
    putStrLn (exercise 5 "Transitive closure (trClos)")

    putStrLn "-- EXAMPLES --"
    putStrLn "Transitive closure of [(1,2),(2,3),(3,4)] is:"
    print (trClos [(1,2),(2,3),(3,4)] :: Rel Int)
    putStrLn "Transitive closure of [(1,2),(2,3),(3,1)] is:"
    print (trClos [(3,1),(2,3),(1,2)] :: Rel Int)
    putStrLn "Transitive closure of [(1,2),(2,1)] is:"
    print (trClos [(1,2),(2,1)] :: Rel Int)
    putStrLn "Transitive closure of [(1,2),(1,2),(2,3)] is:"
    print (trClos [(1,2),(1,2),(2,3)] :: Rel Int)
    putStrLn "Transitive closure of [] is:"
    print (trClos [] :: Rel Int)
