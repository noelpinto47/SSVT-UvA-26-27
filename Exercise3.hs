module Exercise3 where

implies :: Bool -> Bool -> Bool
implies p q = not p || q

stronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\x -> implies (p x) (q x)) xs

weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
weaker xs p q = stronger xs q p

-- Domain specified by the exercise

domain :: [Int]
domain = [1 .. 10]

-- The four properties from Workshop 2 Exercise 3

prop1 :: Int -> Bool
prop1 x = even x && x > 3

prop2 :: Int -> Bool
prop2 x = even x || x > 3

prop3 :: Int -> Bool
prop3 x = (even x && x > 3) || even x

prop4 :: Int -> Bool
prop4 x = even x
