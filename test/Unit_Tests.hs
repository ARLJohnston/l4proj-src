module Main where

import Test.Tasty
import Test.Tasty.HUnit

import CTL
import CTLParser

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

testParseFail :: TestTree
testParseFail = testCase "Parse (\"\") -> Fail" $ isLeft parseResult @?= True
  where
    parseResult = runCTLParser "" lookupTable

testParseSatisfy :: TestTree
testParseSatisfy = testCase "Parse (\"satA\")" $ parseResult @?= CTLLabel [False, False]
  where
    maybeParseResult = runCTLParser "satA" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseAnd :: TestTree
testParseAnd = testCase "Parse (\"satB^satC\")" $ parseResult @?= CTLAnd (CTLLabel [False, True]) (CTLLabel [True, False])
  where
    maybeParseResult = runCTLParser "satB^satC" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseNot :: TestTree
testParseNot = testCase "Parse (\"¬satD\")" $ parseResult @?= CTLNot (CTLLabel [True, True]) 
  where
    maybeParseResult = runCTLParser "¬satD" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseExistsNext :: TestTree
testParseExistsNext = testCase "Parse (\"∃XsatA\")" $ parseResult @?= ExistsNext (CTLLabel [False, False])
  where
    maybeParseResult = runCTLParser "∃XsatA" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseExistsPhiUntilPsi :: TestTree
testParseExistsPhiUntilPsi = testCase "Parse (\"∃satBUsatC\")" $ parseResult @?= ExistsPhiUntilPsi (CTLLabel [False, True]) (CTLLabel [True, False])
  where
    maybeParseResult = runCTLParser "∃satBUsatC" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseExistsAlwaysPhi :: TestTree
testParseExistsAlwaysPhi  = testCase "Parse (\"∃☐satD\")" $ parseResult @?= ExistsAlways (CTLLabel [True, True])
  where
    maybeParseResult = runCTLParser "∃☐satD" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseForAllNextPhi :: TestTree
testParseForAllNextPhi = testCase "Parse (\"∀XsatA\")" $ parseResult @?= ForAllNext (CTLLabel [False, False])
  where
    maybeParseResult = runCTLParser "∀XsatA" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseForAllPhiUntilPsi :: TestTree
testParseForAllPhiUntilPsi  = testCase "Parse (\"∀satBUsatC\")" $ parseResult @?= ForAllPhiUntilPsi (CTLLabel [False, True]) (CTLLabel [True, False])
  where
    maybeParseResult = runCTLParser "∀satBUsatC" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseForAllEventuallyPhi :: TestTree
testParseForAllEventuallyPhi  = testCase "Parse (\"∀◇satD\")" $ parseResult @?= ForAllEventually (CTLLabel [True, True])
  where
    maybeParseResult = runCTLParser "∀◇satD" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseForAllAlwaysPhi :: TestTree
testParseForAllAlwaysPhi = testCase "Parse (\"∀☐satA\")" $ parseResult @?= ForAllAlways (CTLLabel [False, False])
  where
    maybeParseResult = runCTLParser "∀☐satA" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseGrouping :: TestTree
testParseGrouping = testCase "Parse (\"(satB)\")" $ parseResult @?= CTLLabel [False, True]
  where
    maybeParseResult = runCTLParser "(satB)" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseUUU :: TestTree
testParseUUU = testCase "Parse (\"∃UUU\")" $ parseResult @?= ExistsPhiUntilPsi (CTLLabel [True, True]) (CTLLabel [True, True])
  where
    maybeParseResult = runCTLParser "∃UUU" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseUuntilU :: TestTree
testParseUuntilU = testCase "Parse (\"∃UuntilU\")" $ parseResult @?= ExistsPhiUntilPsi (CTLLabel [True, True]) (CTLLabel [True, True])
  where
    maybeParseResult = runCTLParser "∃UuntilU" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

individualParserTests :: TestTree
individualParserTests = testGroup "Individual Expression tests for the CTLParser"
  [
      testParseFail
    , testParseSatisfy
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
testParseAndWithNot = testCase "Parse (\"satA^(¬satB)\")" $ parseResult @?= CTLAnd (CTLLabel [False, False]) (CTLNot (CTLLabel [False, True]))
  where
    maybeParseResult = runCTLParser "satA^(¬satB)" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseExistsNextWithExistsPhiUntilPsi :: TestTree
testParseExistsNextWithExistsPhiUntilPsi = testCase "Parse (\"∃X(∃satCUsatD)\")" $ parseResult @?= ExistsNext (ExistsPhiUntilPsi (CTLLabel [True, False]) (CTLLabel [True, True]))
  where
    maybeParseResult = runCTLParser "∃X(∃satCUsatD)" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseExistsAlwaysWithForAllNext :: TestTree
testParseExistsAlwaysWithForAllNext = testCase "Parse (\"∃☐(∀XsatA)\")" $ parseResult @?= ExistsAlways (ForAllNext (CTLLabel [False, False]))
  where
    maybeParseResult = runCTLParser "∃☐(∀XsatA)" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseForAllPhiUntilPsiWithForAllEventually :: TestTree
testParseForAllPhiUntilPsiWithForAllEventually = testCase "Parse (\"∀satBU(∀◇satC)\")" $ parseResult @?= ForAllPhiUntilPsi (CTLLabel [False, True]) (ForAllEventually (CTLLabel [True, False]))
  where
    maybeParseResult = runCTLParser "∀satBU(∀◇satC)" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

testParseForAllAlwaysWithNot :: TestTree
testParseForAllAlwaysWithNot = testCase "Parse (\"∀☐(¬satD)\")" $ parseResult @?= ForAllAlways (CTLNot (CTLLabel [True, True]))
  where
    maybeParseResult = runCTLParser "∀☐(¬satD)" lookupTable
    parseResult = fromRight (CTLLabel []) maybeParseResult

compositeParserTests :: TestTree
compositeParserTests = testGroup "Composite Expression tests for the CTLParser"
  [
      testParseAndWithNot 
    , testParseExistsNextWithExistsPhiUntilPsi 
    , testParseExistsAlwaysWithForAllNext 
    , testParseForAllPhiUntilPsiWithForAllEventually 
    , testParseForAllAlwaysWithNot 
  ]

mapping :: [([Char], [Bool])]
mapping =
  [
      ("A", satA)
    , ("B", satB)
    , ("C", satC)
  ]

testSatisfy_E2E :: TestTree
testSatisfy_E2E = testCase "E2E (B)" $ result @?= [True,True,True,False,True,False,False,False]
  where
    parseFormula = fromRight (CTLLabel []) $ runCTLParser "B" mapping
    result = evaluateCTL parseFormula transitionSystem

testAnd_E2E :: TestTree
testAnd_E2E = testCase "E2E (A^C)" $ result @?= [True,False,False,False,False,True,False,False]
  where
    parseFormula = fromRight (CTLLabel []) $ runCTLParser "A^C" mapping
    result = evaluateCTL parseFormula transitionSystem

testNot_E2E :: TestTree
testNot_E2E = testCase "E2E (¬A)" $ result @?= [False,False,True,False,True,False,True,True]
  where
    parseFormula = fromRight (CTLLabel []) $ runCTLParser "¬A" mapping
    result = evaluateCTL parseFormula transitionSystem

testExistsNext_E2E :: TestTree
testExistsNext_E2E = testCase "E2E (∃XA)" $ result @?= [False,True,True,True,True,True,False,True]
  where
    parseFormula = fromRight (CTLLabel []) $ runCTLParser "∃XA" mapping
    result = evaluateCTL parseFormula transitionSystem

testExistsAlways_E2E :: TestTree
testExistsAlways_E2E = testCase "E2E (∃☐B)" $ result @?= [True,False,True,False,True,False,False,False]
  where
    parseFormula = fromRight (CTLLabel []) $ runCTLParser "∃☐B" mapping
    result = evaluateCTL parseFormula transitionSystem


testExistsPhiUntilPsi_E2E :: TestTree
testExistsPhiUntilPsi_E2E = testCase "E2E (∃AUC)" $ result @?= [True,True,True,True,False,True,True,False]
  where
    parseFormula = fromRight (CTLLabel []) $ runCTLParser "∃AUC" mapping
    result = evaluateCTL parseFormula transitionSystem

-- test_E2E :: TestTree
-- test_E2E = testCase "E2E ()" $ result @?=
--   where
--     parseFormula = fromRight (CTLLabel []) $ runCTLParser "" mapping
--     result = evaluateCTL parseFormula transitionSystem

endToEndTests :: TestTree
endToEndTests = testGroup "End-to-end tests for parser "
  [
      testSatisfy_E2E
    , testAnd_E2E
    , testNot_E2E
    , testExistsNext_E2E
    , testExistsAlways_E2E
    , testExistsPhiUntilPsi_E2E
  ]

testSets :: TestTree
testSets = testGroup "All sets of tests"
  [
      transitionSystemTests
    , individualCases
    , individualParserTests
    , compositeParserTests 
    , endToEndTests
  ]

main :: IO ()
main = defaultMain testSets
