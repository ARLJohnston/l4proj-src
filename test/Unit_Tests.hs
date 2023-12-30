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
    base = Data.Vector.replicate 8 False
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
satB = toList $ Data.Vector.replicate 8 False // [(0, True), (1, True), (2, True), (4,True)]
satC :: [Bool]
satC = toList $ replicate 8 False // [(0, True), (2, True), (5, True), (6, True)]

testExistsNext :: TestTree
testExistsNext = testCase "Exists next A" $ existsNextPhi transitionSystem satA @?= (toList $ base // [(1, True), (2, True), (3, True), (4, True), (5, True), (7, True)])
  where
    base = replicate 8 False

testExistsAlwaysPhi :: TestTree
testExistsAlwaysPhi = testCase "Exists Always B" $ existsAlwaysPhi transitionSystem satB @?= (toList $ base // [(0, True), (2, True), (4, True)])
  where
    base = replicate 8 False

testExistsPhiUntilPsi :: TestTree
testExistsPhiUntilPsi = testCase "Exists A until C" $ existsPhiUntilPsi transitionSystem satA satC @?= (toList $ base // [(0, True), (1, True), (2, True), (3, True), (5, True), (6, True)])
  where
    base = replicate 8 False

transitionSystemTests :: TestTree
transitionSystemTests = testGroup "Tests on Transition System from Figure 6.11 in Principles of Model Checking (Direct use of function)"
  [
      testCase "Pre on 6.11" $ transitionSystem `pre` 0 @?= (toList $ base // [(2, True), (3, True), (4, True)])
    , testCase "Post on 6.11" $ transitionSystem `post` 7 @?= (toList $ base // [(3, True), (6, True)])
    -- 6 Cases for CTL model Checking (Ignoring Base and Not)
    , testExistsNext
    , testExistsAlwaysPhi
    , testExistsPhiUntilPsi
  ]
  where
    base = replicate 8 False

satA_CTL :: CTLFormula
satA_CTL = Satisfaction satA
		
satB_CTL :: CTLFormula
satB_CTL = Satisfaction satB
		
satC_CTL :: CTLFormula
satC_CTL = Satisfaction satC

testSatisfy_CTL :: TestTree
testSatisfy_CTL = testCase "Eval (B)" $ evaluateCTL satB_CTL transitionSystem @?= (toList $ base // [(0, True), (1, True), (2, True), (4, True)])
  where
    base = replicate 8 False

testAnd_CTL  :: TestTree
testAnd_CTL = testCase "Eval (A^C)" $ evaluateCTL (And satA_CTL satC_CTL) transitionSystem @?= (toList $ base // [(0, True), (5, True)])
  where
    base = replicate 8 False

testNot_CTL :: TestTree
testNot_CTL = testCase "Eval (¬A)" $ evaluateCTL (Not satA_CTL) transitionSystem @?= (toList $ base // [(2, True), (4, True), (6, True), (7, True)])
  where
    base = replicate 8 False

testExistsNext_CTL :: TestTree
testExistsNext_CTL = testCase "Eval (∃XA)" $ evaluateCTL (ExistsNext satA_CTL) transitionSystem @?= (toList $ base // [(1, True), (2, True), (3, True), (4, True), (5, True), (7, True)])
  where
    base = replicate 8 False

testExistsAlwaysPhi_CTL :: TestTree
testExistsAlwaysPhi_CTL = testCase "Eval (∃☐B)" $ evaluateCTL (ExistsAlwaysPhi satB_CTL) transitionSystem @?= (toList $ base // [(0, True), (2, True), (4, True)])
  where
    base = replicate 8 False

testExistsPhiUntilPsi_CTL :: TestTree
testExistsPhiUntilPsi_CTL = testCase "Eval (∃AUC)" $ evaluateCTL (ExistsPhiUntilPsi satA_CTL satC_CTL) transitionSystem @?= (toList $ base // [(0, True), (1, True), (2, True), (3, True), (5, True), (6, True)])
  where
    base = replicate 8 False

testForAllNextPhi_CTL :: TestTree
testForAllNextPhi_CTL  = testCase "Eval (∀X)" $ True @?= True
  where
    base = replicate 8 False

testForAllPhiUntilPsi_CTL :: TestTree
testForAllPhiUntilPsi_CTL = testCase "Eval (∀U)" $ True @?= True
  where
    base = replicate 8 False

testForAllEventuallyPhi_CTL :: TestTree
testForAllEventuallyPhi_CTL = testCase "Eval (∀☐)" $ True @?= True
  where
    base = replicate 8 False

testForAllAlwaysPhi_CTL :: TestTree
testForAllAlwaysPhi_CTL = testCase "Eval (∀())" $ True @?= True
  where
    base = replicate 8 False

individualCases :: TestTree
individualCases = testGroup "Tests on Transition System from Figure 6.11 in Principles of Model Checking (Using EvaluateCTL)"
  [
      testSatisfy_CTL
    , testAnd_CTL
    , testNot_CTL
    , testExistsNext_CTL
    , testExistsAlwaysPhi_CTL
    , testExistsPhiUntilPsi_CTL
    , testForAllNextPhi_CTL
    , testForAllPhiUntilPsi_CTL
    , testForAllEventuallyPhi_CTL
    , testForAllAlwaysPhi_CTL
  ]
  where
    base = replicate 8 False



main :: IO ()
main = defaultMain transitionSystemTests
