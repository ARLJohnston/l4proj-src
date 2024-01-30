{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck (quickCheckAll)
import Control.Monad
import System.Exit (exitSuccess, exitFailure)

import CTL

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
      ctlFormula' 0 = liftM Satisfy arbitrary
      ctlFormula' n | n > 0 =
        oneof
          [
              liftM  Satisfy arbitrary
            , liftM  Atomic arbitrary
            , liftM2 And phi psi
            , liftM2 Or phi psi
            , liftM  Not phi
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
prop_DualAllNextPhi phi m = evaluateCTL (ForAllNext phi) m == evaluateCTL (Not (ExistsNext (Not phi))) m

prop_DualAllEventuallyPhi :: CTLFormula -> [[Bool]] -> Bool
prop_DualAllEventuallyPhi phi m = evaluateCTL (ForAllEventually phi) m == evaluateCTL (Not (ExistsAlways (Not phi))) m

prop_DualExistsNextPhi :: CTLFormula -> [[Bool]] -> Bool
prop_DualExistsNextPhi phi m = evaluateCTL (ExistsNext phi) m == evaluateCTL (Not (ForAllNext (Not phi))) m

prop_DualExistsEventuallyPhi :: CTLFormula -> [[Bool]] -> Bool
prop_DualExistsEventuallyPhi phi m = evaluateCTL (ExistsEventually phi) m == evaluateCTL (Not (ForAllAlways (Not phi))) m

prop_DualForAllPhiUntilPsi :: CTLFormula -> CTLFormula -> [[Bool]] -> Bool
prop_DualForAllPhiUntilPsi phi psi m = evaluateCTL (ForAllPhiUntilPsi phi psi) m == evaluateCTL (And (Not (ExistsPhiUntilPsi (Not psi) (And (Not phi) (Not psi)))) (Not (ExistsAlways (Not psi)))) m

prop_ExpandForAllPhiUntilPsi :: CTLFormula -> CTLFormula -> [[Bool]] -> Bool
prop_ExpandForAllPhiUntilPsi phi psi m = evaluateCTL (ForAllPhiUntilPsi phi psi) m == evaluateCTL (Or psi (And (phi) (ForAllNext (ForAllPhiUntilPsi phi psi)))) m

prop_ExpandForAllEventaullyPhi :: CTLFormula -> [[Bool]] -> Bool
prop_ExpandForAllEventaullyPhi phi m = evaluateCTL (ForAllEventually phi) m == evaluateCTL (Or phi (ForAllNext (ForAllEventually phi))) m

prop_ExpandForAllAlwaysPhi :: CTLFormula -> [[Bool]] -> Bool
prop_ExpandForAllAlwaysPhi phi m = evaluateCTL (ForAllAlways phi) m == evaluateCTL (And phi (ForAllNext (ForAllAlways phi))) m

prop_ExpandExistsPhiUntilPsi :: CTLFormula -> CTLFormula -> [[Bool]] -> Bool
prop_ExpandExistsPhiUntilPsi phi psi m = evaluateCTL (ExistsPhiUntilPsi phi psi) m == evaluateCTL (Or psi (And phi (ExistsNext (ExistsPhiUntilPsi phi psi)))) m

prop_ExpandExistsEventuallyPhi :: CTLFormula -> [[Bool]] -> Bool
prop_ExpandExistsEventuallyPhi phi m = evaluateCTL (ExistsEventually phi) m == evaluateCTL (Or phi (ExistsNext (ExistsEventually phi))) m

prop_ExpandExistsAlwaysPhi :: CTLFormula -> [[Bool]] -> Bool
prop_ExpandExistsAlwaysPhi phi m = evaluateCTL (ExistsAlways phi) m == evaluateCTL (And phi (ExistsNext (ExistsAlways phi))) m

prop_DistForAllAlwaysPhiAndPsi :: CTLFormula -> CTLFormula -> [[Bool]] -> Bool
prop_DistForAllAlwaysPhiAndPsi phi psi m = evaluateCTL (ForAllAlways (And phi psi)) m == evaluateCTL (And (ForAllAlways phi) (ForAllAlways psi)) m

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
