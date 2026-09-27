module Exercise3 where
    
import Data.List

type Rel a = [(a,a)]

-- Symmetric closure of a relation
symClos :: Ord a => Rel a -> Rel a
symClos r = sort $ r ++ [(y,x) | (x,y) <- r]