module Exercise2 where

import Data.List
import System.Random
import SetOrd
import Test.QuickCheck

-- TODO: Also use scratch generator from Exercise1

genIntSet :: Gen (Set Int)
genIntSet = fmap toSet (listOf arbitrary :: Gen [Int])

-- Build a Set from a list: sort and remove duplicates
toSet :: Ord a => [a] -> Set a
toSet = Set . sort . nub

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set a) (Set b) = toSet $ filter (\x -> x `elem` b) a

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = toSet $ a ++ b

-- not graded but there is a more efficient solution because they are ordered
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = toSet $ filter (\x -> not $ x `elem` b) a


-- Int domain properties

prop_commutativityUnion :: Property
prop_commutativityUnion = forAll genIntSet (\a -> forAll genIntSet (\b -> setUnion a b == setUnion b a))

prop_associativity :: Property
prop_associativity = forAll genIntSet (\a ->
    forAll genIntSet (\b -> 
        forAll genIntSet (\c ->
            setIntersection c (setIntersection a b) == setIntersection a (setIntersection b c)
            )
        )
    )

prop_idempotence :: Property
prop_idempotence = forAll genIntSet (\a -> setIntersection a a == a)

prop_absorption :: Property
prop_absorption = forAll genIntSet (\a -> setIntersection a emptySet == emptySet)

prop_distributivity :: Property
prop_distributivity = forAll genIntSet (\a ->
    forAll genIntSet (\b -> 
        forAll genIntSet (\c ->
            setIntersection a (setUnion b c) == setUnion (setIntersection a b) (setIntersection a c)
            )
        )
    )

-- Needed for prop_cardinality
getSetLength :: Set a -> Int
getSetLength (Set []) = 0
getSetLength (Set (x:xs)) = 1 + getSetLength(Set (xs))

prop_cardinality :: Property
prop_cardinality = forAll genIntSet (\a ->
    forAll genIntSet (\b -> 
            getSetLength (setIntersection a b) == getSetLength a + getSetLength b - getSetLength (setUnion a b)
        )
    )

-- Float domain properties

genFloatSet :: Gen (Set Float)
genFloatSet = fmap toSet (listOf arbitrary :: Gen [Float])

prop_commutativityUnionFloat :: Property
prop_commutativityUnionFloat = forAll genFloatSet (\a -> forAll genFloatSet (\b -> setUnion a b == setUnion b a))

prop_associativityFloat :: Property
prop_associativityFloat = forAll genFloatSet (\a ->
    forAll genFloatSet (\b ->
        forAll genFloatSet (\c ->
            setIntersection c (setIntersection a b) == setIntersection a (setIntersection b c)
            )
        )
    )

prop_idempotenceFloat :: Property
prop_idempotenceFloat = forAll genFloatSet (\a -> setIntersection a a == a)

prop_absorptionFloat :: Property
prop_absorptionFloat = forAll genFloatSet (\a -> setIntersection a emptySet == emptySet)

prop_distributivityFloat :: Property
prop_distributivityFloat = forAll genFloatSet (\a ->
    forAll genFloatSet (\b ->
        forAll genFloatSet (\c ->
            setIntersection a (setUnion b c) == setUnion (setIntersection a b) (setIntersection a c)
            )
        )
    )

prop_cardinalityFloat :: Property
prop_cardinalityFloat = forAll genFloatSet (\a ->
    forAll genFloatSet (\b ->
            getSetLength (setIntersection a b) == getSetLength a + getSetLength b - getSetLength (setUnion a b)
        )
    )

main :: IO ()
main = do
    putStrLn "Int domain quickCheck generator"
    quickCheck prop_commutativityUnion
    quickCheck prop_associativity
    quickCheck prop_idempotence
    quickCheck prop_absorption
    quickCheck prop_distributivity
    quickCheck prop_cardinality

    putStrLn "Float domain quickCheck generator"
    quickCheck prop_commutativityUnionFloat
    quickCheck prop_associativityFloat
    quickCheck prop_idempotenceFloat
    quickCheck prop_absorptionFloat
    quickCheck prop_distributivityFloat
    quickCheck prop_cardinalityFloat