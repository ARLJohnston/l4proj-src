module CTL (module CTL) where

import Data.Matrix (Matrix, getCol, getRow, nrows)
import Data.Vector (toList)
import Data.List (nub, findIndices)

import Control.Parallel.Strategies

--satPhi :: [Bool]
--satPhi = [True, False, True]

--matrix :: Matrix Bool
--matrix = fromLists [[False, True, True], [False, False, True], [False, False, False]]
--ts = fromLists [[False, True], [False, False]]

pre :: Matrix a -> Int -> [a]
pre m n = toList $ getCol (n+1) m

post :: Matrix a -> Int -> [a] 
post m n = toList $ getRow (n+1) m

data CTLFormula =
    Satisfaction [Bool]
  | Atomic [Bool]
  | And CTLFormula CTLFormula
  | Not CTLFormula
  | ExistsNext CTLFormula
  | ExistsPhiUntilPsi CTLFormula CTLFormula
  | ExistsAlways CTLFormula
  | ForAllNext CTLFormula
  | ForAllPhiUntilPsi CTLFormula CTLFormula
  | ForAllEventually CTLFormula
  | ForAllAlways CTLFormula
    deriving (Eq)

instance Show CTLFormula where
  show (Satisfaction satisfy) = "Sat(" ++ show satisfy ++ ")"
  show (Atomic satisfy) = "Atom(" ++ show satisfy ++ ")"
  show (And phi psi) = "(" ++ show phi ++ ") ^ (" ++ show psi ++ ")"
  show (Not phi) = "¬(" ++ show phi ++ ")"
  show (ExistsNext phi) = "∃X(" ++ show phi ++ ")" 
  show (ExistsPhiUntilPsi phi psi) = "∃((" ++ show phi ++ ") U (" ++ show psi ++ "))"
  show (ExistsAlways phi) = "∃☐(" ++ show phi ++ ")"
  show (ForAllNext phi) = "∀X(" ++ show phi ++ ")"
  show (ForAllPhiUntilPsi phi psi) = "∀((" ++ show phi ++ ") U (" ++ show psi ++ "))"
  show (ForAllEventually phi) = "∀◇(" ++ show phi ++ ")"
  show (ForAllAlways phi) = "∀☐(" ++ show phi ++ ")"

evaluateCTL :: CTLFormula -> Matrix Bool -> [Bool]
evaluateCTL (Satisfaction satisfy) _ = satisfy
evaluateCTL (Atomic satisfy) _ = satisfy
evaluateCTL (And phi psi) m = zipWith (&&) (evaluateCTL phi m) (evaluateCTL psi m) `using` parList rseq
evaluateCTL (Not phi) m = map not (evaluateCTL phi m) `using` parList rseq
evaluateCTL (ExistsNext phi) m = existsNextPhi m (evaluateCTL phi m) `using` parList rseq
evaluateCTL (ExistsPhiUntilPsi phi psi) m = existsPhiUntilPsi m (evaluateCTL phi m) (evaluateCTL psi m)
evaluateCTL (ExistsAlways phi) m = existsAlwaysPhi m (evaluateCTL phi m)

evaluateCTL (ForAllNext phi) m = map not (existsNextPhi m notPhi) `using` parList rseq
  where
    notPhi = map not (evaluateCTL phi m) `using` parList rseq

evaluateCTL (ForAllPhiUntilPsi phi psi) m = zipWith (&&) doesNotExistCombo doesNotExistNotPsi `using` parList rseq
  where 
    notPhi = map not (evaluateCTL phi m) `using` parList rseq
    notPsi = map not (evaluateCTL psi m) `using` parList rseq
    notPhiAndNotPsi = zipWith (&&) notPhi notPsi `using` parList rseq
    doesNotExistNotPsi = map not (existsAlwaysPhi m notPsi) `using` parList rseq
    doesNotExistCombo = map not (existsPhiUntilPsi m notPsi notPhiAndNotPsi) `using` parList rseq

evaluateCTL (ForAllEventually phi) m = map not (existsAlwaysPhi m notPhi) `using` parList rseq
  where
    notPhi = map not (evaluateCTL phi m) `using` parList rseq

evaluateCTL (ForAllAlways phi) m = map not (existsPhiUntilPsi m true notPhi) `using` parList rseq
  where
    notPhi = map not (evaluateCTL phi m) `using` parList rseq
    true = replicate (nrows m) True 

existsNextPhi :: Matrix Bool -> [Bool] -> [Bool]
existsNextPhi matrix satisfy = stepByFunc satisfy [True | _ <- [0.. length satisfy -1]] matrix pre

--satPsi :: [Bool]
--satPsi = [False, False, True]

extendBy :: [Bool] -> (Matrix Bool -> Int -> [Bool]) -> Matrix Bool -> [Int]
extendBy prior step m = posterior
  where
    vertices = findIndices id prior
    vertices' = map (step m) vertices `using` parList rseq
    posterior = nub $ [ vv | uu <- map (findIndices id) vertices', vv <- uu]

stepByFunc :: [Bool] -> [Bool] -> Matrix Bool -> (Matrix Bool -> Int -> [Bool]) -> [Bool]
stepByFunc [] _ _ _ = []
stepByFunc prior labelling m step = posterior
  where
--States we can reach
    vertices  = extendBy prior step m
--Filter to states where the predicate is true
    reachable = filter (labelling !!) vertices
    posterior = [x `elem` reachable | x <- [0..length prior - 1]]

existsPhiUntilPsi :: Matrix Bool -> [Bool] -> [Bool] -> [Bool]
existsPhiUntilPsi _ [] satisfy = satisfy
existsPhiUntilPsi _ _ [] = []
existsPhiUntilPsi matrix satPhi satisfy =
  if satisfy' == satisfy
    then satisfy
    else existsPhiUntilPsi matrix satPhi satisfy'
  where
    nextStep = stepByFunc satisfy satPhi matrix pre
    satisfy' = zipWith (||) satisfy nextStep `using` parList rseq

existsAlwaysPhi :: Matrix Bool -> [Bool] -> [Bool]
existsAlwaysPhi _ [] = []
existsAlwaysPhi matrix satisfy =
  if satisfy' == satisfy
    then satisfy
    else existsAlwaysPhi matrix satisfy'
  where
    nextStep = stepByFunc satisfy satisfy matrix pre
    satisfy' = zipWith (&&) satisfy nextStep `using` parList rseq
