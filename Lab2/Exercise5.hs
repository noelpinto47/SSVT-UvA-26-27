module Exercise5 where

import Test.QuickCheck
import Data.List (nub)
import GHC.OldList (union)
import Data.List (sort)

type Rel a = [(a, a)]
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- trClos :: Ord a => Rel a -> Rel a

step :: Ord a => Rel a -> Rel a -> Rel a
step r current = current `union` (current @@ r)


trClos :: Ord a => Rel a -> Rel a
trClos r = loop r r
  where
    loop r current = 
        let next = current `union` (current @@ r)
        in if sort next == sort current
           then next
           else loop r next

-- trClos r = loop r r — start the loop with current = r (the original relation is always part of its own closure)
-- let next = current union (current @@ r) — compute one round: existing pairs + newly reachable pairs
-- if sort next == sort current — if nothing changed (after sorting to ignore order), we're done
-- then next — return the fixed point
-- else loop r next — otherwise, continue with next as the new current

