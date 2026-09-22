module Exercise8 where

import SetOrd
import Test.QuickCheck
import Lecture3
import Data.List (nub)

sub :: Form -> Set Form
sub f@(Prop x) = Set [f]
sub f@(Neg g) = unionSet (Set [f]) (sub g)
sub f@(Cnj fs) = foldl unionSet (Set [f]) (map sub fs)
sub f@(Dsj fs) = foldl unionSet (Set [f]) (map sub fs)
sub f@(Impl f1 f2) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)


-- Counts the EXACT number of distinct sub-formulae
nsub :: Form -> Int
nsub f = fst (go (Set []) f)
  where
    go :: Set Form -> Form -> (Int, Set Form)
    go seen f
        | inSet f seen = (0, seen)

        | otherwise =
            let seen' = insertSet f seen
            in case f of
                Prop _ ->
                    (1, seen')

                Neg g ->
                    let (n, seen'') = go seen' g
                    in (1 + n, seen'')

                Cnj fs ->
                    let (n, seen'') = visitList seen' fs
                    in (1 + n, seen'')

                Dsj fs ->
                    let (n, seen'') = visitList seen' fs
                    in (1 + n, seen'')

                Impl f1 f2 ->
                    let (n1, seen1) = go seen' f1
                        (n2, seen2) = go seen1 f2
                    in (1 + n1 + n2, seen2)

                Equiv f1 f2 ->
                    let (n1, seen1) = go seen' f1
                        (n2, seen2) = go seen1 f2
                    in (1 + n1 + n2, seen2)

    visitList :: Set Form -> [Form] -> (Int, Set Form)
    visitList seen [] = (0, seen)

    visitList seen (f:fs) =
        let (n1, seen1) = go seen f
            (n2, seen2) = visitList seen1 fs
        in (n1 + n2, seen2)


subList :: Form -> [Form]
subList f@(Prop _)      = [f]
subList f@(Neg g)       = f : subList g
subList f@(Cnj fs)      = f : concatMap subList fs
subList f@(Dsj fs)      = f : concatMap subList fs
subList f@(Impl f1 f2)  = f : subList f1 ++ subList f2
subList f@(Equiv f1 f2) = f : subList f1 ++ subList f2


-- Generator for random propositional formulas
formGen :: Gen Form
formGen = sized gen
  where
    gen 0 =
        oneof
            [ Prop <$> choose (0, 5)
            , return (Cnj [])
            , return (Dsj [])
            ]

    gen n =
        frequency
            [ (3, Prop <$> choose (0, 5))
            , (2, Neg <$> gen (n `div` 2))
            , (2, do
                    k <- choose (0, 3)
                    fs <- vectorOf k (gen (n `div` 2))
                    return (Cnj fs))
            , (2, do
                    k <- choose (0, 3)
                    fs <- vectorOf k (gen (n `div` 2))
                    return (Dsj fs))
            , (2, Impl <$> gen (n `div` 2) <*> gen (n `div` 2))
            , (2, Equiv <$> gen (n `div` 2) <*> gen (n `div` 2))
            ]

-- Structural shrinker
shrinkForm :: Form -> [Form]
shrinkForm (Prop x)      = [Prop x' | x' <- shrink x, x' >= 0]
shrinkForm (Neg f)       = f : [Neg f' | f' <- shrinkForm f]
shrinkForm (Cnj fs)      = fs ++ [Cnj fs' | fs' <- shrinkList shrinkForm fs]
shrinkForm (Dsj fs)      = fs ++ [Dsj fs' | fs' <- shrinkList shrinkForm fs]
shrinkForm (Impl f1 f2)  = [f1, f2] ++ [Impl f1' f2 | f1' <- shrinkForm f1] ++ [Impl f1 f2' | f2' <- shrinkForm f2]
shrinkForm (Equiv f1 f2) = [f1, f2] ++ [Equiv f1' f2 | f1' <- shrinkForm f1] ++ [Equiv f1 f2' | f2' <- shrinkForm f2]


-- to write `forAll formGen`, so failures shrink to a minimal case.
forAllForm :: Testable prop => (Form -> prop) -> Property
forAllForm = forAllShrink formGen shrinkForm


-- Independent definition:
-- "g is a sub-formula of f"
isSubFormula :: Form -> Form -> Bool
isSubFormula g f | g == f = True
isSubFormula g (Neg f) = isSubFormula g f
isSubFormula g (Cnj fs) = any (isSubFormula g) fs
isSubFormula g (Dsj fs) = any (isSubFormula g) fs
isSubFormula g (Impl f1 f2) = isSubFormula g f1 || isSubFormula g f2
isSubFormula g (Equiv f1 f2) = isSubFormula g f1 || isSubFormula g f2
isSubFormula _ (Prop _) = False


-- Helper (SetOrd has no built-in size function)
setSize :: Set a -> Int
setSize (Set xs) = length xs


-- QuickCheck property 1
-- Every formula is a sub-formula of itself.
prop_subContainsItself :: Property
prop_subContainsItself = forAllForm $ \f -> inSet f (sub f)


-- QuickCheck property 2
-- sub f should contain exactly the formulas that are structurally
-- sub-formulae of f.
prop_subCorrect :: Property
prop_subCorrect =
    forAllForm $ \f ->
        let Set xs = sub f
        in forAll (oneof [formGen, elements xs]) $ \g -> classify (isSubFormula g f) "positive case" $ inSet g (sub f) == isSubFormula g f


-- QuickCheck property 3 for nsub
-- nsub must equal the number of elements in sub.
prop_nsubMatchesSub :: Property
prop_nsubMatchesSub = forAllForm $ \f -> nsub f == setSize (sub f)


-- Cross-checks nsub against subList, which is built
-- without touching SetOrd at all. This is the independent check that
-- was missing: prop_nsubMatchesSub alone couldn't catch a bug shared
-- between sub's unionSet and nsub's insertSet/inSet.
prop_nsubMatchesIndependentCount :: Property
prop_nsubMatchesIndependentCount =
    forAllForm $ \f -> nsub f == length (nub (subList f))


-- QuickCheck property 4 for nsub
-- There is always at least one sub-formula: the formula itself.
prop_nsubPositive :: Property
prop_nsubPositive = forAllForm $ \f -> nsub f >= 1

main :: IO ()
main = do
    putStrLn (exercise 8 "Sub-formulae (sub, nsub)")

    putStrLn "-- sub --"
    quickCheckWith stdArgs { maxSuccess = 500 } prop_subContainsItself
    quickCheckWith stdArgs { maxSuccess = 500 } prop_subCorrect

    putStrLn ""
    putStrLn "-- nsub --"
    quickCheckWith stdArgs { maxSuccess = 500 } prop_nsubMatchesSub
    quickCheckWith stdArgs { maxSuccess = 500 } prop_nsubMatchesIndependentCount
    quickCheckWith stdArgs { maxSuccess = 500 } prop_nsubPositive