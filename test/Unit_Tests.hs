module Main where

import Test.Tasty
import Test.Tasty.HUnit

import CTL

import Prelude hiding (replicate)
import Data.Matrix hiding (toList)
import Data.Matrix (Matrix, fromLists, fromList, getCol, getRow, prettyMatrix, nrows, ncols, matrix)
import Data.Vector
import Data.Vector (Vector, toList, replicate)
import Data.List (nub, findIndices, intersect, union)
import Data.Bool

transitionSystem :: Matrix Bool
transitionSystem = ts
  where
    base = replicate 8 False
    s_0 = rowVector (base // [(2, True)])
    s_1 = rowVector (base // [(3, True)])
    s_2 = rowVector (base // [(0, True), (1, True)])
    s_3 = rowVector (base // [(0, True)])
    s_4 = rowVector (base // [(0, True),(1, True)])
    s_5 = rowVector (base // [(1, True),(7, True)])
    s_6 = rowVector (base // [(4, True)])
    s_7 = rowVector (base // [(3, True), (6,True)])
    ts = s_0 <-> s_1 <-> s_2 <-> s_3 <-> s_4 <-> s_5 <-> s_6 <-> s_7

satA :: [Bool]
satA = toList $ replicate 8 False // [(0, True), (1, True), (3, True), (5, True)]
satB :: [Bool]
satB = toList $ replicate 8 False // [(0, True), (1, True), (2, True), (4,True)]
satC :: [Bool]
satC = toList $ replicate 8 False // [(0, True), (2, True), (5, True), (6, True)]

testExistsAlwaysPhi :: TestTree
testExistsAlwaysPhi  = testCase "Exists Always B" $ existsAlwaysPhi transitionSystem satB @?= (toList $ base // [(0, True), (2, True), (4, True)])
  where
    base = replicate 8 False
    

transitionSystemTests :: TestTree
transitionSystemTests = testGroup "Tests on Transition System from Figure 6.11 in Principles of Model Checking"
  [
      testCase "2+2" $ 2 + 2 @?= 4
    , testCase "Pre on 6.11" $ transitionSystem `pre` 0 @?= (toList $ base // [(2, True), (3, True), (4, True)])
    , testCase "Post on 6.11" $ transitionSystem `post` 7 @?= (toList $ base // [(3, True), (6, True)])
    -- 6 Cases for CTL model Checking
    -- , testCase "" $ @?=
    -- , testCase "" $ @?=
    -- , testCase "" $ @?=
    -- , testCase "" $ @?=
    , testExistsAlwaysPhi
    , testCase "Exists A until C" $ existsPhiUntilPsi transitionSystem satA satC @?= (toList $ base // [(0, True), (1, True), (3, True), (4, True)])
  ]
  where
    base = replicate 8 False

main :: IO ()
main = defaultMain transitionSystemTests
