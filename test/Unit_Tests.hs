module Main where

import Test.Tasty
import Test.Tasty.HUnit

import CTL
import CTLParser

import LTL
import LTLParser

import Data.Either (fromRight, isLeft)

transitionSystem :: [[Bool]]
transitionSystem =
  [
    [False,False,True,False,False,False,False,False],
    [False,False,False,True,False,False,False,False],
    [True,True,False,False,False,False,False,False],
    [True,False,False,False,False,False,False,False],
    [True,True,False,False,False,False,False,False],
    [False,True,False,False,False,False,False,True],
    [False,False,False,False,True,False,False,False],
    [False,False,False,True,False,False,True,False]
  ]

satA :: [Bool]
satA = [True,True,False,True,False,True,False,False]

satB :: [Bool]
satB = [True,True,True,False,True,False,False,False]

satC :: [Bool]
satC = [True,False,True,False,False,True,True,False]

tarjan :: [[Bool]]
tarjan =
  [
    [False, True, False, False, False, False, False],
    [False, False, True, False, True, True, False],
    [False, False, False, True, False, True, False],
    [False, False, False, False, False, False, True],
    [True, False, False, False, False, True, False],
    [False, False, True, False, False, False, True],
    [False, False, True, True, False, False, False]
  ]

testExistsAlwaysPhi :: TestTree
testExistsAlwaysPhi = testCase "Exists Always B" $ existsAlwaysPhi transitionSystem satB @?= [True,False,True,False,True,False,False,False]

testExistsPhiUntilPsi :: TestTree
testExistsPhiUntilPsi = testCase "Exists A until C" $ existsPhiUntilPsi transitionSystem satA satC @?= [True,True,True,True,False,True,True,False]

transitionSystemTests :: TestTree
transitionSystemTests = testGroup "Tests on Transition System from Figure 6.11 in Principles of Model Checking (Direct use of function)"
  [
      testCase "Pre on 6.11" $ transitionSystem `pre` 0 @?= [False,False,True,True,True,False,False,False]
    , testCase "Post on 6.11" $ transitionSystem `post` 7 @?= [False,False,False,True,False,False,True,False]
    , testExistsAlwaysPhi
    , testExistsPhiUntilPsi
    , testCase "DFS" $ depthFirstSearch transitionSystem [] 0 @?= [3, 1, 2, 0]
    , testCase "Tarjan's Algorithm" $ getSCCs tarjan @?= [[2, 3, 6, 5],[0, 1, 4]]
  ]

satA_CTL :: CTLFormula
satA_CTL = CTLLabel satA

satB_CTL :: CTLFormula
satB_CTL = CTLLabel satB

satC_CTL :: CTLFormula
satC_CTL = CTLLabel satC

testSatisfy_CTL :: TestTree
testSatisfy_CTL = testCase "Eval (B)" $ evaluateCTL satB_CTL transitionSystem @?= [True,True,True,False,True,False,False,False]

testAnd_CTL  :: TestTree
testAnd_CTL = testCase "Eval (A^C)" $ evaluateCTL (CTLAnd satA_CTL satC_CTL) transitionSystem @?= [True,False,False,False,False,True,False,False]

testNot_CTL :: TestTree
testNot_CTL = testCase "Eval (¬A)" $ evaluateCTL (CTLNot satA_CTL) transitionSystem @?= [False,False,True,False,True,False,True,True]

testExistsNext_CTL :: TestTree
testExistsNext_CTL = testCase "Eval (∃XA)" $ evaluateCTL (ExistsNext satA_CTL) transitionSystem @?= [False,True,True,True,True,True,False,True]

testExistsAlwaysPhi_CTL :: TestTree
testExistsAlwaysPhi_CTL = testCase "Eval (∃☐B)" $ evaluateCTL (ExistsAlways satB_CTL) transitionSystem @?= [True,False,True,False,True,False,False,False]

testExistsPhiUntilPsi_CTL :: TestTree
testExistsPhiUntilPsi_CTL = testCase "Eval (∃AUC)" $ evaluateCTL (ExistsPhiUntilPsi satA_CTL satC_CTL) transitionSystem @?= [True,True,True,True,False,True,True,False]

testForAllNextPhi_CTL :: TestTree
testForAllNextPhi_CTL  = testCase "Eval (∀XA)" $ True @?= True

testForAllPhiUntilPsi_CTL :: TestTree
testForAllPhiUntilPsi_CTL = testCase "Eval (∀BUC)" $ True @?= True

testForAllEventuallyPhi_CTL :: TestTree
testForAllEventuallyPhi_CTL = testCase "Eval (∀◇(A))" $ True @?= True

testForAllAlwaysPhi_CTL :: TestTree
testForAllAlwaysPhi_CTL = testCase "Eval (∀☐(B))" $ True @?= True

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



lookupTable :: [([Char], [Bool])]
lookupTable =
  [
      ("satA", [False, False])
    , ("satB", [False, True])
    , ("satC", [True, False])
    , ("satD", [True, True])
    , ("U",    [True, True])
  ]

testCTLParseFail :: TestTree
testCTLParseFail = testCase "Parse (\"\") -> Fail" $ isLeft parseResult @?= True
  where
    parseResult = runCTLParser "" lookupTable

testCTLParseSatisfy :: TestTree
testCTLParseSatisfy = testCase "Parse (\"satA\")" $ parseResult @?= CTLLabel [False, False]
  where
    maybeParseResult = runCTLParser "satA" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseAnd :: TestTree
testCTLParseAnd = testCase "Parse (\"satB^satC\")" $ parseResult @?= CTLAnd (CTLLabel [False, True]) (CTLLabel [True, False])
  where
    maybeParseResult = runCTLParser "satB^satC" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseNot :: TestTree
testCTLParseNot = testCase "Parse (\"¬satD\")" $ parseResult @?= CTLNot (CTLLabel [True, True])
  where
    maybeParseResult = runCTLParser "¬satD" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseExistsNext :: TestTree
testCTLParseExistsNext = testCase "Parse (\"∃XsatA\")" $ parseResult @?= ExistsNext (CTLLabel [False, False])
  where
    maybeParseResult = runCTLParser "∃XsatA" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseExistsPhiUntilPsi :: TestTree
testCTLParseExistsPhiUntilPsi = testCase "Parse (\"∃satBUsatC\")" $ parseResult @?= ExistsPhiUntilPsi (CTLLabel [False, True]) (CTLLabel [True, False])
  where
    maybeParseResult = runCTLParser "∃satBUsatC" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseExistsAlwaysPhi :: TestTree
testCTLParseExistsAlwaysPhi  = testCase "Parse (\"∃☐satD\")" $ parseResult @?= ExistsAlways (CTLLabel [True, True])
  where
    maybeParseResult = runCTLParser "∃☐satD" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseForAllNextPhi :: TestTree
testCTLParseForAllNextPhi = testCase "Parse (\"∀XsatA\")" $ parseResult @?= ForAllNext (CTLLabel [False, False])
  where
    maybeParseResult = runCTLParser "∀XsatA" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseForAllPhiUntilPsi :: TestTree
testCTLParseForAllPhiUntilPsi  = testCase "Parse (\"∀satBUsatC\")" $ parseResult @?= ForAllPhiUntilPsi (CTLLabel [False, True]) (CTLLabel [True, False])
  where
    maybeParseResult = runCTLParser "∀satBUsatC" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseForAllEventuallyPhi :: TestTree
testCTLParseForAllEventuallyPhi  = testCase "Parse (\"∀◇satD\")" $ parseResult @?= ForAllEventually (CTLLabel [True, True])
  where
    maybeParseResult = runCTLParser "∀◇satD" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseForAllAlwaysPhi :: TestTree
testCTLParseForAllAlwaysPhi = testCase "Parse (\"∀☐satA\")" $ parseResult @?= ForAllAlways (CTLLabel [False, False])
  where
    maybeParseResult = runCTLParser "∀☐satA" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseGrouping :: TestTree
testCTLParseGrouping = testCase "Parse (\"(satB)\")" $ parseResult @?= CTLLabel [False, True]
  where
    maybeParseResult = runCTLParser "(satB)" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseUUU :: TestTree
testCTLParseUUU = testCase "Parse (\"∃UUU\")" $ parseResult @?= ExistsPhiUntilPsi (CTLLabel [True, True]) (CTLLabel [True, True])
  where
    maybeParseResult = runCTLParser "∃UUU" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseUuntilU :: TestTree
testCTLParseUuntilU = testCase "Parse (\"∃UuntilU\")" $ parseResult @?= ExistsPhiUntilPsi (CTLLabel [True, True]) (CTLLabel [True, True])
  where
    maybeParseResult = runCTLParser "∃UuntilU" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

individualParserTests :: TestTree
individualParserTests = testGroup "Individual Expression tests for the CTLParser"
  [
      testCTLParseFail
    , testCTLParseSatisfy
    , testCTLParseAnd
    , testCTLParseNot
    , testCTLParseExistsNext
    , testCTLParseExistsPhiUntilPsi
    , testCTLParseExistsAlwaysPhi
    , testCTLParseForAllNextPhi
    , testCTLParseForAllPhiUntilPsi
    , testCTLParseForAllEventuallyPhi
    , testCTLParseForAllAlwaysPhi
    , testCTLParseGrouping
  ]

testCTLParseAndWithNot :: TestTree
testCTLParseAndWithNot = testCase "Parse (\"satA^(¬satB)\")" $ parseResult @?= CTLAnd (CTLLabel [False, False]) (CTLNot (CTLLabel [False, True]))
  where
    maybeParseResult = runCTLParser "satA^(¬satB)" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseExistsNextWithExistsPhiUntilPsi :: TestTree
testCTLParseExistsNextWithExistsPhiUntilPsi = testCase "Parse (\"∃X(∃satCUsatD)\")" $ parseResult @?= ExistsNext (ExistsPhiUntilPsi (CTLLabel [True, False]) (CTLLabel [True, True]))
  where
    maybeParseResult = runCTLParser "∃X(∃satCUsatD)" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseExistsAlwaysWithForAllNext :: TestTree
testCTLParseExistsAlwaysWithForAllNext = testCase "Parse (\"∃☐(∀XsatA)\")" $ parseResult @?= ExistsAlways (ForAllNext (CTLLabel [False, False]))
  where
    maybeParseResult = runCTLParser "∃☐(∀XsatA)" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseForAllPhiUntilPsiWithForAllEventually :: TestTree
testCTLParseForAllPhiUntilPsiWithForAllEventually = testCase "Parse (\"∀satBU(∀◇satC)\")" $ parseResult @?= ForAllPhiUntilPsi (CTLLabel [False, True]) (ForAllEventually (CTLLabel [True, False]))
  where
    maybeParseResult = runCTLParser "∀satBU(∀◇satC)" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testCTLParseForAllAlwaysWithNot :: TestTree
testCTLParseForAllAlwaysWithNot = testCase "Parse (\"∀☐(¬satD)\")" $ parseResult @?= ForAllAlways (CTLNot (CTLLabel [True, True]))
  where
    maybeParseResult = runCTLParser "∀☐(¬satD)" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

compositeParserTests :: TestTree
compositeParserTests = testGroup "Composite Expression tests for the CTLParser"
  [
      testCTLParseAndWithNot
    , testCTLParseExistsNextWithExistsPhiUntilPsi
    , testCTLParseExistsAlwaysWithForAllNext
    , testCTLParseForAllPhiUntilPsiWithForAllEventually
    , testCTLParseForAllAlwaysWithNot
  ]

-- testLTLParseFail :: TestTree
-- testLTLParseFail = testCase "Parse (\"\") -> Fail" $ isLeft parseResult @?= True
--   where
--     parseResult = runLTLParser "" lookupTable

testLTLParseSatisfy :: TestTree
testLTLParseSatisfy = testCase "Parse (\"satA\")" $ parseResult @?= LTLLabel [False, False]
  where
    maybeParseResult = runLTLParser "satA" lookupTable
    parseResult = fromRight (LTLLabel []) maybeParseResult

testLTLParseAnd :: TestTree
testLTLParseAnd = testCase "Parse (\"satB^satC\")" $ parseResult @?= LTLAnd (LTLLabel [False, True]) (LTLLabel [True, False])
  where
    maybeParseResult = runLTLParser "satB^satC" lookupTable
    parseResult = fromRight (LTLLabel []) maybeParseResult

testLTLParseNot :: TestTree
testLTLParseNot = testCase "Parse (\"¬satD\")" $ parseResult @?= LTLNot (LTLLabel [True, True])
  where
    maybeParseResult = runLTLParser "¬satD" lookupTable
    parseResult = fromRight (LTLLabel []) maybeParseResult

testLTLParseNext :: TestTree
testLTLParseNext = testCase "Parse (\"XsatA\")" $ parseResult @?= Next (LTLLabel [False, False])
  where
    maybeParseResult = runLTLParser "XsatA" lookupTable
    parseResult = fromRight (LTLLabel []) maybeParseResult

-- testLTLParseUntil :: TestTree
-- testLTLParseUntil = testCase "Parse (\"satBUsatC\")" $ parseResult @?= Until (LTLLabel [False, True]) (LTLLabel [True, False])
--   where
--     maybeParseResult = runLTLParser "satBUsatC" lookupTable
--     parseResult = fromRight (LTLLabel []) maybeParseResult


individualLTLParserTests :: TestTree
individualLTLParserTests = testGroup "Individual Expression tests for the LTLParser"
  [
      --testLTLParseFail
      testLTLParseSatisfy
    , testLTLParseAnd
    , testLTLParseNot
    , testLTLParseNext
    -- , testLTLParseUntil
  ]

mapping :: [([Char], [Bool])]
mapping =
  [
      ("A", satA)
    , ("B", satB)
    , ("C", satC)
  ]

testSatisfy_E2E_CTL :: TestTree
testSatisfy_E2E_CTL = testCase "E2E_CTL (B)" $ result @?= [True,True,True,False,True,False,False,False]
  where
    parseFormula = fromRight (CTLLabel []) $ runCTLParser "B" mapping
    result = evaluateCTL parseFormula transitionSystem

testAnd_E2E_CTL :: TestTree
testAnd_E2E_CTL = testCase "E2E_CTL (A^C)" $ result @?= [True,False,False,False,False,True,False,False]
  where
    parseFormula = fromRight (CTLLabel []) $ runCTLParser "A^C" mapping
    result = evaluateCTL parseFormula transitionSystem

testNot_E2E_CTL :: TestTree
testNot_E2E_CTL = testCase "E2E_CTL (¬A)" $ result @?= [False,False,True,False,True,False,True,True]
  where
    parseFormula = fromRight (CTLLabel []) $ runCTLParser "¬A" mapping
    result = evaluateCTL parseFormula transitionSystem

testExistsNext_E2E_CTL :: TestTree
testExistsNext_E2E_CTL = testCase "E2E_CTL (∃XA)" $ result @?= [False,True,True,True,True,True,False,True]
  where
    parseFormula = fromRight (CTLLabel []) $ runCTLParser "∃XA" mapping
    result = evaluateCTL parseFormula transitionSystem

testExistsAlways_E2E_CTL :: TestTree
testExistsAlways_E2E_CTL = testCase "E2E_CTL (∃☐B)" $ result @?= [True,False,True,False,True,False,False,False]
  where
    parseFormula = fromRight (CTLLabel []) $ runCTLParser "∃☐B" mapping
    result = evaluateCTL parseFormula transitionSystem


testExistsPhiUntilPsi_E2E_CTL :: TestTree
testExistsPhiUntilPsi_E2E_CTL = testCase "E2E_CTL (∃AUC)" $ result @?= [True,True,True,True,False,True,True,False]
  where
    parseFormula = fromRight (CTLLabel []) $ runCTLParser "∃AUC" mapping
    result = evaluateCTL parseFormula transitionSystem

-- test_E2E_CTL :: TestTree
-- test_E2E_CTL = testCase "E2E_CTL ()" $ result @?=
--   where
--     parseFormula = fromRight (CTLLabel []) $ runCTLParser "" mapping
--     result = evaluateCTL parseFormula transitionSystem

endToEndTests :: TestTree
endToEndTests = testGroup "End-to-end tests for CTLParser "
  [
      testSatisfy_E2E_CTL
    , testAnd_E2E_CTL
    , testNot_E2E_CTL
    , testExistsNext_E2E_CTL
    , testExistsAlways_E2E_CTL
    , testExistsPhiUntilPsi_E2E_CTL
  ]

testSets :: TestTree
testSets = testGroup "All sets of tests"
  [
      transitionSystemTests
    , individualCases
    , individualParserTests
    , compositeParserTests
    , endToEndTests
    , individualLTLParserTests
  ]

main :: IO ()
main = defaultMain testSets
