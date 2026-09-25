module Exercise7 where

import Data.List
import System.Random
import SetOrd
import Test.QuickCheck

-- TODO: Replace these algorithms with the ones from Exercise 3 and 5 

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos r = fp r
  where
    fp current
      | current == next = current
      | otherwise       = fp next
      where next = nub (sort (current ++ (current @@ r)))

symClos :: Ord a => Rel a -> Rel a
symClos relation = nub $ ( map (\(a,b) -> (b,a)) relation ++ relation)

main :: IO ()
main = do

  let r = [(1,2),(2,3)]
  putStrLn $ "Original: " ++ show r

  -- Counterexample link to my onenote where I calculated the counterexample with diagrams
  -- Section: Lab2 - Exercise7
  -- https://amsuni-my.sharepoint.com/:o:/g/personal/fabian_baischer_student_uva_nl/IgCbVeEpj_7_SbxyMmy49UWwAYYFRaDFNqNqLopF71kyyys?e=7Re7UG

  -- Time spent: ~3 hours

  -- First calculate symmetric closure and then transitive closure
  let symmentric_closure1     = symClos r
  let result1 = trClos symmentric_closure1
  putStrLn "\nFirst calculate symmetric closure and then transitive closure"
  putStrLn $ "After symClos:        " ++ show symmentric_closure1
  putStrLn $ "  Added by symClos:   " ++ show (symmentric_closure1 \\ r)
  -- symmentric_closure1 \\ r  all pairs that are in symmentric_closure1 but NOT in r
  putStrLn $ "After trClos:         " ++ show result1
  putStrLn $ "  Added by trClos:    " ++ show (result1 \\ symmentric_closure1)
  putStrLn $ "result1 = " ++ show result1

  -- Second calculate transitive closure and then symmetric closure
  let transitive_closure2     = trClos r
  let result2 = symClos transitive_closure2
  putStrLn "\nSecond calculate transitive closure and then symmetric closure"
  putStrLn $ "After trClos:         " ++ show transitive_closure2
  putStrLn $ "  Added by trClos:    " ++ show (transitive_closure2 \\ r)
  putStrLn $ "After symClos:        " ++ show result2
  putStrLn $ "  Added by symClos:   " ++ show (result2 \\ transitive_closure2)
  putStrLn $ "result2 = " ++ show result2

  -- Result
  putStrLn "\nResult"
  putStrLn $ "result1 = " ++ show result1
  putStrLn $ "result2 = " ++ show result2
  putStrLn $ "result1 == result2: " ++ show (result1 == result2)