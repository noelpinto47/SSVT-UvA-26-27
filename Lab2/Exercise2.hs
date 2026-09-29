module Exercise2 where

import Data.List
import System.Random
import SetOrd
import Test.QuickCheck

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

-- generates a random set of integers with size 0-20, and values (-10)-10
scratchRandomSetGen :: Gen (Set Int)
scratchRandomSetGen = do
    size <- choose (0, 20)
    randomValues <- sequence (replicate size (choose (-10, 10)))
    return (list2set randomValues)

-- Additional property tests using the scratch generator from Exercise1
prop_commutativityUnionExercise1 :: Property
prop_commutativityUnionExercise1 = forAll scratchRandomSetGen (\a -> forAll scratchRandomSetGen (\b -> setUnion a b == setUnion b a))

prop_associativityExercise1 :: Property
prop_associativityExercise1 = forAll scratchRandomSetGen (\a ->
    forAll scratchRandomSetGen (\b -> 
        forAll scratchRandomSetGen (\c ->
            setIntersection c (setIntersection a b) == setIntersection a (setIntersection b c)
            )
        )
    )

prop_idempotenceExercise1 :: Property
prop_idempotenceExercise1 = forAll scratchRandomSetGen (\a -> setIntersection a a == a)

prop_absorptionExercise1 :: Property
prop_absorptionExercise1 = forAll scratchRandomSetGen (\a -> setIntersection a emptySet == emptySet)

prop_distributivityExercise1 :: Property
prop_distributivityExercise1 = forAll scratchRandomSetGen (\a ->
    forAll scratchRandomSetGen (\b ->
        forAll scratchRandomSetGen (\c ->
            setIntersection a (setUnion b c) == setUnion (setIntersection a b) (setIntersection a c)
            )
        )
    )

prop_cardinalityExercise1 :: Property
prop_cardinalityExercise1 = forAll scratchRandomSetGen (\a ->
    forAll scratchRandomSetGen (\b ->
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

    putStrLn "Using scratch generator from Exercise1"
    quickCheck prop_commutativityUnionExercise1
    quickCheck prop_associativityExercise1
    quickCheck prop_idempotenceExercise1
    quickCheck prop_absorptionExercise1
    quickCheck prop_distributivityExercise1
    quickCheck prop_cardinalityExercise1