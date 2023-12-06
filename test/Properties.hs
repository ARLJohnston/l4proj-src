{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck (quickCheckAll)
import Control.Monad
import System.Exit (exitSuccess, exitFailure)
import CTL

import Data.Matrix (Matrix, fromLists, fromList, getCol, getRow, prettyMatrix, nrows, ncols)
import Data.Vector (Vector, toList)
import Data.List (nub, findIndices, intersect, union)
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

--instance Arbitrary (Matrix Bool -> Int -> [Bool]) where
--  arbitrary = oneof [return pre, return post]

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

prop_ExtendByInRangePre :: [Bool] -> Matrix Bool -> Bool
prop_ExtendByInRangePre prior m = length (filter (\x -> x >= 0 && x <= nrows m) posterior) == length posterior
  where
    posterior = extendBy prior pre m

prop_ExtendByInRangePost :: [Bool] -> Matrix Bool -> Bool
prop_ExtendByInRangePost prior m = length (filter (\x -> x >= 0 && x <= nrows m) posterior) == length posterior
  where
    posterior = extendBy prior post m

-- Template Haskell requires this line for use of quickCheckAll
$(return [])

main :: IO ()
main = do
  putStrLn "Running Tests"
  success <- $(quickCheckAll)
  if success then
    putStrLn "All tests passed!" >> exitSuccess 
  else
    putStrLn "Some tests failed." >> exitFailure
