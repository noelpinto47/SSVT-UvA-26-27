module Exercise5 where

import Test.QuickCheck
import Data.List (nub)

type Rel a = [(a, a)]
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]
