module Main where

import Test.Tasty
import Test.Tasty.HUnit

import CTL

import Data.Matrix (Matrix, fromLists, fromList, getCol, getRow, prettyMatrix, nrows, ncols)
import Data.Vector (Vector, toList)
import Data.List (nub, findIndices, intersect, union)
import Data.Bool

tests :: TestTree
tests = testGroup "Tests"
  [
    testCase "Matix ncols == 3" $ ncols matrix @?= 3
  ]

main :: IO ()
main = defaultMain tests
