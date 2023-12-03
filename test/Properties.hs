{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

import Test.QuickCheck

import Data.Matrix (Matrix, fromLists, getCol, getRow, prettyMatrix)
import Data.Vector (Vector, toList)
import Data.List (nub, findIndices, intersect, union, isSubsequenceOf)
import Data.Bool

instance Arbitrary (Matrix Bool) where
  arbitrary = do
    -- Transition System required to be square
    n <- getSize
    elts <- vectorOf (n*n) arbitrary
    return $ fromList n n elts

instance {-# OVERLAPPING #-} Arbitrary [Bool] where
  arbitrary = do
    n <- getSize
    vectorOf n arbitrary

instance {-# OVERLAPPING #-} Arbitrary (Matrix a -> Int -> [a]) where
  arbitrary = oneof [return pre, return post]

prop_TSIsSquare :: Matrix Bool -> Bool
prop_TSIsSquare m = nrows m == ncols m

-- Check that generator for [Bool] works
prop_BoolList :: [Bool] -> Bool
prop_BoolList [] = True
prop_BoolList (x:xs) = if (x /= True && x /= False) then False else prop_BoolList xs

prop_TSRowLenEquivStatesLen :: [Bool] -> Matrix Bool -> Int -> Bool
prop_TSRowLenEquivStatesLen sat ts = \n ->
    if n > 0 && n < length sat
        then length sat == length (getRow n ts)
        else True

prop_TSColLenEquivStatesLen :: [Bool] -> Matrix Bool -> Int -> Bool
prop_TSColLenEquivStatesLen sat ts = \n ->
    if n > 0 && n < length sat
        then length sat == length (getCol n ts)
        else True

prop_ExtendByReturnsInRange :: [Bool] -> [Bool] -> Matrix Bool -> (Matrix a -> Int -> [a]) -> Bool
prop_ExtendByReturnsInRange prior sat ts transition = foldr (&&) True inRange
  where
    posterior = stepByFunc prior sat ts transition
    inRange = map (\x -> x > 0 && x <= nrows ts) posterior
