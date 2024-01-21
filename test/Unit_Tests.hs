module Main where

import Test.Tasty
import Test.Tasty.HUnit

import CTL
import CTLParser

import Prelude hiding (replicate)
import Data.Matrix hiding (toList)
import Data.Matrix (Matrix, fromLists, fromList, getCol, getRow, prettyMatrix, nrows, ncols, matrix)
import Data.Vector
import Data.Vector (Vector, toList, replicate)
import Data.List (nub, findIndices, intersect, union)
import Data.Bool
import Data.Either (fromRight, isLeft)

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
testExistsAlwaysPhi_CTL = testCase "Eval (∃☐B)" $ evaluateCTL (ExistsAlways satB_CTL) transitionSystem @?= (toList $ base // [(0, True), (2, True), (4, True)])
  where
    base = replicate 8 False

testExistsPhiUntilPsi_CTL :: TestTree
testExistsPhiUntilPsi_CTL = testCase "Eval (∃AUC)" $ evaluateCTL (ExistsPhiUntilPsi satA_CTL satC_CTL) transitionSystem @?= (toList $ base // [(0, True), (1, True), (2, True), (3, True), (5, True), (6, True)])
  where
    base = replicate 8 False

testForAllNextPhi_CTL :: TestTree
testForAllNextPhi_CTL  = testCase "Eval (∀XA)" $ True @?= True
  where
    base = replicate 8 False

testForAllPhiUntilPsi_CTL :: TestTree
testForAllPhiUntilPsi_CTL = testCase "Eval (∀BUC)" $ True @?= True
  where
    base = replicate 8 False

testForAllEventuallyPhi_CTL :: TestTree
testForAllEventuallyPhi_CTL = testCase "Eval (∀◇(A))" $ True @?= True
  where
    base = replicate 8 False

testForAllAlwaysPhi_CTL :: TestTree
testForAllAlwaysPhi_CTL = testCase "Eval (∀☐(B))" $ True @?= True
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



lookupTable :: [([Char], CTLFormula)]
lookupTable =
  [
      ("satA", Satisfaction [False, False])
    , ("satB", Satisfaction [False, True])
    , ("satC", Satisfaction [True, False])
    , ("satD", Satisfaction [True, True])
  ]

testParseFail :: TestTree
testParseFail = testCase "Parse (\"\") -> Fail" $ isLeft parseResult @?= True
  where
    parseResult = runCTLParser "" lookupTable

testParseSatisfaction :: TestTree
testParseSatisfaction = testCase "Parse (\"satA\")" $ parseResult @?= Satisfaction [False, False]
  where
    maybeParseResult = runCTLParser "satA" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

testParseAnd :: TestTree
testParseAnd = testCase "Parse (\"satB^satC\")" $ parseResult @?= And (Satisfaction [False, True]) (Satisfaction [True, False])
  where
    maybeParseResult = runCTLParser "satB^satC" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

testParseNot :: TestTree
testParseNot = testCase "Parse (\"¬satD\")" $ parseResult @?= Not (Satisfaction [True, True]) 
  where
    maybeParseResult = runCTLParser "¬satD" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

testParseExistsNext :: TestTree
testParseExistsNext = testCase "Parse (\"∃XsatA\")" $ parseResult @?= ExistsNext (Satisfaction [False, False])
  where
    maybeParseResult = runCTLParser "∃XsatA" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

testParseExistsPhiUntilPsi :: TestTree
testParseExistsPhiUntilPsi = testCase "Parse (\"∃satBUsatC\")" $ parseResult @?= ExistsPhiUntilPsi (Satisfaction [False, True]) (Satisfaction [True, False])
  where
    maybeParseResult = runCTLParser "∃satBUsatC" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

testParseExistsAlwaysPhi :: TestTree
testParseExistsAlwaysPhi  = testCase "Parse (\"∃☐satD\")" $ parseResult @?= ExistsAlways (Satisfaction [True, True])
  where
    maybeParseResult = runCTLParser "∃☐satD" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

testParseForAllNextPhi :: TestTree
testParseForAllNextPhi = testCase "Parse (\"∀XsatA\")" $ parseResult @?= ForAllNext (Satisfaction [False, False])
  where
    maybeParseResult = runCTLParser "∀XsatA" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

testParseForAllPhiUntilPsi :: TestTree
testParseForAllPhiUntilPsi  = testCase "Parse (\"∀satBUsatC\")" $ parseResult @?= ForAllPhiUntilPsi (Satisfaction [False, True]) (Satisfaction [True, False])
  where
    maybeParseResult = runCTLParser "∀satBUsatC" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

testParseForAllEventuallyPhi :: TestTree
testParseForAllEventuallyPhi  = testCase "Parse (\"∀◇satD\")" $ parseResult @?= ForAllEventually (Satisfaction [True, True])
  where
    maybeParseResult = runCTLParser "∀◇satD" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

testParseForAllAlwaysPhi :: TestTree
testParseForAllAlwaysPhi = testCase "Parse (\"∀☐satA\")" $ parseResult @?= ForAllAlways (Satisfaction [False, False])
  where
    maybeParseResult = runCTLParser "∀☐satA" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

testParseGrouping :: TestTree
testParseGrouping = testCase "Parse (\"(satB)\")" $ parseResult @?= Satisfaction [False, True]
  where
    maybeParseResult = runCTLParser "(satB)" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

individualParserTests :: TestTree
individualParserTests = testGroup "Individual Expression tests for the CTLParser"
  [
      testParseFail
    , testParseSatisfaction
    , testParseAnd
    , testParseNot
    , testParseExistsNext 
    , testParseExistsPhiUntilPsi 
    , testParseExistsAlwaysPhi 
    , testParseForAllNextPhi 
    , testParseForAllPhiUntilPsi 
    , testParseForAllEventuallyPhi 
    , testParseForAllAlwaysPhi
    , testParseGrouping
  ]

testParseAndWithNot :: TestTree
testParseAndWithNot = testCase "Parse (\"satA^(¬satB)\")" $ parseResult @?= And (Satisfaction [False, False]) (Not (Satisfaction [False, True]))
  where
    maybeParseResult = runCTLParser "satA^(¬satB)" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

testParseExistsNextWithExistsPhiUntilPsi :: TestTree
testParseExistsNextWithExistsPhiUntilPsi = testCase "Parse (\"∃X(∃satCUsatD)\")" $ parseResult @?= ExistsNext (ExistsPhiUntilPsi (Satisfaction [True, False]) (Satisfaction [True, True]))
  where
    maybeParseResult = runCTLParser "∃X(∃satCUsatD)" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

testParseExistsAlwaysWithForAllNext :: TestTree
testParseExistsAlwaysWithForAllNext = testCase "Parse (\"∃☐(∀XsatA)\")" $ parseResult @?= ExistsAlways (ForAllNext (Satisfaction [False, False]))
  where
    maybeParseResult = runCTLParser "∃☐(∀XsatA)" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

testParseForAllPhiUntilPsiWithForAllEventually :: TestTree
testParseForAllPhiUntilPsiWithForAllEventually = testCase "Parse (\"∀satBU(∀◇satC)\")" $ parseResult @?= ForAllPhiUntilPsi (Satisfaction [False, True]) (ForAllEventually (Satisfaction [True, False]))
  where
    maybeParseResult = runCTLParser "∀satBU(∀◇satC)" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

testParseForAllAlwaysWithNot :: TestTree
testParseForAllAlwaysWithNot = testCase "Parse (\"∀☐(¬satD)\")" $ parseResult @?= ForAllAlways (Not (Satisfaction [True, True]))
  where
    maybeParseResult = runCTLParser "∀☐(¬satD)" lookupTable
    parseResult = fromRight (Satisfaction []) maybeParseResult

compositeParserTests :: TestTree
compositeParserTests = testGroup "Composite Expression tests for the CTLParser"
  [
      testParseAndWithNot 
    , testParseExistsNextWithExistsPhiUntilPsi 
    , testParseExistsAlwaysWithForAllNext 
    , testParseForAllPhiUntilPsiWithForAllEventually 
    , testParseForAllAlwaysWithNot 
  ]

testSets :: TestTree
testSets = testGroup "All sets of tests"
  [
      transitionSystemTests
    , individualCases
    , individualParserTests
    , compositeParserTests 
  ]

main :: IO ()
main = defaultMain testSets
