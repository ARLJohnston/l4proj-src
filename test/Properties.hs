{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck (quickCheckAll)
import Control.Monad
import System.Exit (exitSuccess, exitFailure)

import CTL
import LTL
import TransitionSystem

import Data.List (nub, findIndices, intersect, union)
import Data.Bool

instance {-# OVERLAPPING #-} Arbitrary [[Bool]] where
  arbitrary = do
    -- Transition System required to be square
    n <- getSize
    vectorOf n arbitrary

instance {-# OVERLAPPING #-} Arbitrary [Bool] where
  arbitrary = do
    n <- getSize
    vectorOf n arbitrary

instance Arbitrary CTLFormula where
  arbitrary = sized ctlFormula'
    where
      ctlFormula' 0 = liftM CTLLabel arbitrary
      ctlFormula' n | n > 0 =
	oneof
	  [
	      liftM  CTLLabel arbitrary
	    , liftM  CTLAtom arbitrary
	    , liftM2 CTLAnd phi psi
	    , liftM2 Or phi psi
	    , liftM  CTLNot phi
	    , liftM  ExistsNext phi
	    , liftM2 ExistsPhiUntilPsi phi psi
	    , liftM  ExistsAlways phi
	    , liftM  ExistsEventually phi
	    , liftM  ForAllNext phi
	    , liftM2 ForAllPhiUntilPsi phi psi
	    , liftM  ForAllEventually phi
	    , liftM  ForAllAlways phi
	  ]
	where
	phi = ctlFormula' (n `div` 2)
	psi = ctlFormula' (n `div` 2)

prop_TSIsSquare :: [[Bool]] -> Bool
prop_TSIsSquare m = if (length m) > 0 then length m == length (m !! 0) else True

-- Check that generator for [Bool] works
prop_BoolList :: [Bool] -> Bool
prop_BoolList [] = True
prop_BoolList (x:xs) = if (x /= True && x /= False) then False else prop_BoolList xs

prop_TSRowLenEquivStatesLen :: [Bool] -> [[Bool]] -> Int -> Bool
prop_TSRowLenEquivStatesLen sat ts = \n ->
    if n > 0 && n < length sat
	then length sat == length (ts !! n)
	else True

prop_TSColLenEquivStatesLen :: [Bool] -> [[Bool]]-> Int -> Bool
prop_TSColLenEquivStatesLen sat ts = \n ->
    if n > 0 && n < length sat
	then length sat == length (pre ts n)
	else True

prop_TarjanLeq :: [[Bool]] -> Bool
prop_TarjanLeq ts = length (getSCCs ts) <= length ts

prop_ExtendByInRangePre :: [Bool] -> [[Bool]] -> Bool
prop_ExtendByInRangePre prior m = length (filter (\x -> x >= 0 && x <= (length m)) posterior) == length posterior
  where
    posterior = extendBy prior pre m

prop_ExtendByInRangePost :: [Bool] -> [[Bool]] -> Bool
prop_ExtendByInRangePost prior m = length (filter (\x -> x >= 0 && x <= (length m)) posterior) == length posterior
  where
    posterior = extendBy prior post m

prop_EvalReturnsSameLength :: CTLFormula -> [[Bool]] -> Bool
prop_EvalReturnsSameLength formula m = length (evaluateCTL formula m) == length m

prop_DualAllNextPhi :: CTLFormula -> [[Bool]] -> Bool
prop_DualAllNextPhi phi m = evaluateCTL (ForAllNext phi) m == evaluateCTL (CTLNot (ExistsNext (CTLNot phi))) m

prop_DualAllEventuallyPhi :: CTLFormula -> [[Bool]] -> Bool
prop_DualAllEventuallyPhi phi m = evaluateCTL (ForAllEventually phi) m == evaluateCTL (CTLNot (ExistsAlways (CTLNot phi))) m

prop_DualExistsNextPhi :: CTLFormula -> [[Bool]] -> Bool
prop_DualExistsNextPhi phi m = evaluateCTL (ExistsNext phi) m == evaluateCTL (CTLNot (ForAllNext (CTLNot phi))) m

prop_DualExistsEventuallyPhi :: CTLFormula -> [[Bool]] -> Bool
prop_DualExistsEventuallyPhi phi m = evaluateCTL (ExistsEventually phi) m == evaluateCTL (CTLNot (ForAllAlways (CTLNot phi))) m

prop_DualForAllPhiUntilPsi :: CTLFormula -> CTLFormula -> [[Bool]] -> Bool
prop_DualForAllPhiUntilPsi phi psi m = evaluateCTL (ForAllPhiUntilPsi phi psi) m == evaluateCTL (CTLAnd (CTLNot (ExistsPhiUntilPsi (CTLNot psi) (CTLAnd (CTLNot phi) (CTLNot psi)))) (CTLNot (ExistsAlways (CTLNot psi)))) m

prop_ExpandForAllPhiUntilPsi :: CTLFormula -> CTLFormula -> [[Bool]] -> Bool
prop_ExpandForAllPhiUntilPsi phi psi m = evaluateCTL (ForAllPhiUntilPsi phi psi) m == evaluateCTL (Or psi (CTLAnd (phi) (ForAllNext (ForAllPhiUntilPsi phi psi)))) m

prop_ExpandForAllEventaullyPhi :: CTLFormula -> [[Bool]] -> Bool
prop_ExpandForAllEventaullyPhi phi m = evaluateCTL (ForAllEventually phi) m == evaluateCTL (Or phi (ForAllNext (ForAllEventually phi))) m

prop_ExpandForAllAlwaysPhi :: CTLFormula -> [[Bool]] -> Bool
prop_ExpandForAllAlwaysPhi phi m = evaluateCTL (ForAllAlways phi) m == evaluateCTL (CTLAnd phi (ForAllNext (ForAllAlways phi))) m

prop_ExpandExistsPhiUntilPsi :: CTLFormula -> CTLFormula -> [[Bool]] -> Bool
prop_ExpandExistsPhiUntilPsi phi psi m = evaluateCTL (ExistsPhiUntilPsi phi psi) m == evaluateCTL (Or psi (CTLAnd phi (ExistsNext (ExistsPhiUntilPsi phi psi)))) m

prop_ExpandExistsEventuallyPhi :: CTLFormula -> [[Bool]] -> Bool
prop_ExpandExistsEventuallyPhi phi m = evaluateCTL (ExistsEventually phi) m == evaluateCTL (Or phi (ExistsNext (ExistsEventually phi))) m

prop_ExpandExistsAlwaysPhi :: CTLFormula -> [[Bool]] -> Bool
prop_ExpandExistsAlwaysPhi phi m = evaluateCTL (ExistsAlways phi) m == evaluateCTL (CTLAnd phi (ExistsNext (ExistsAlways phi))) m

prop_DistForAllAlwaysPhiAndPsi :: CTLFormula -> CTLFormula -> [[Bool]] -> Bool
prop_DistForAllAlwaysPhiAndPsi phi psi m = evaluateCTL (ForAllAlways (CTLAnd phi psi)) m == evaluateCTL (CTLAnd (ForAllAlways phi) (ForAllAlways psi)) m

prop_DistExistsEventuallyPhiOrPsi :: CTLFormula -> CTLFormula -> [[Bool]] -> Bool
prop_DistExistsEventuallyPhiOrPsi phi psi m = evaluateCTL (ExistsEventually (Or phi psi)) m == evaluateCTL (Or (ExistsEventually phi) (ExistsEventually psi)) m

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
