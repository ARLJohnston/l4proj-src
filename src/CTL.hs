module CTL(module CTL) where

import Data.Matrix (Matrix, fromLists, getCol, getRow, prettyMatrix, nrows, ncols)
import Data.Vector (Vector, toList)
import Data.List (nub, findIndices, elem)
import Data.Bool

import Control.Parallel.Strategies

--satPhi :: [Bool]
--satPhi = [True, False, True]

--matrix :: Matrix Bool
matrix = fromLists [[False, True, True], [False, False, True], [False, False, False]]
ts = fromLists [[False, True], [False, False]]

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
  | ExistsAlwaysPhi CTLFormula
    deriving (Eq, Show)

evaluateCTL :: CTLFormula -> Matrix Bool -> [Bool]
evaluateCTL (Satisfaction satisfy) _ = satisfy
evaluateCTL (Atomic satisfy) _ = satisfy
evaluateCTL (And phi psi) m = zipWith (&&) (evaluateCTL phi m) (evaluateCTL psi m)
evaluateCTL (Not phi) m = map not (evaluateCTL phi m)
evaluateCTL (ExistsNext phi) m = existsNextPhi m (evaluateCTL phi m)
evaluateCTL (ExistsPhiUntilPsi phi psi) m = existsPhiUntilPsi m (evaluateCTL phi m) (evaluateCTL psi m)
evaluateCTL (ExistsAlwaysPhi phi) m = existsAlwaysPhi m (evaluateCTL phi m)

predicateAnd :: [Bool] -> [Bool] -> [Bool]
predicateAnd satPhi satPsi = [(satPhi !! x) && (satPsi !! x) | x <- [0..length satPhi - 1]]

existsNextPhi :: Matrix Bool -> [Bool] -> [Bool]
existsNextPhi matrix satisfy = stepByFunc satisfy [True | _ <- [0.. length satisfy -1]] matrix pre

--satPsi :: [Bool]
--satPsi = [False, False, True]

extendBy :: [Bool] -> (Matrix Bool -> Int -> [Bool]) -> Matrix Bool -> [Int]
extendBy prior step m = posterior
  where
    vertices = findIndices id prior
    vertices' = map (step m) vertices `using` parBuffer 1 rseq
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
existsPhiUntilPsi matrix [] satisfy = satisfy
existsPhiUntilPsi matrix satPhi [] = []
existsPhiUntilPsi matrix satPhi satisfy =
  if satisfy' == satisfy
    then satisfy
    else existsPhiUntilPsi matrix satPhi satisfy'
  where
    nextStep = stepByFunc satisfy satPhi matrix pre
    satisfy' = zipWith (||) satisfy nextStep

existsAlwaysPhi :: Matrix Bool -> [Bool] -> [Bool]
existsAlwaysPhi matrix [] = []
existsAlwaysPhi matrix satisfy =
  if satisfy' == satisfy
    then satisfy
    else existsAlwaysPhi matrix satisfy'
  where
    nextStep = stepByFunc satisfy satisfy matrix pre
    satisfy' = zipWith (&&) satisfy nextStep
