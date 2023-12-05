module CTL(module CTL) where

import Data.Matrix (Matrix, fromLists, getCol, getRow, prettyMatrix, nrows, ncols)
import Data.Vector (Vector, toList)
import Data.List (nub, findIndices, intersect, union)
import Data.Bool

satPhi :: [Bool]
satPhi = [True, False, True]

matrix :: Matrix Bool
matrix = fromLists [[False, True, True],
                    [False, False, True],
                    [False, False, False]]

pre :: Matrix a -> Int -> [a]
pre m n = toList $ getCol (n+1) m

post :: Matrix a -> Int -> [a] 
post m n = toList $ getRow (n+1) m

data CTLLogicFormula =
    T
  | AP
  | And (CTLLogicFormula) (CTLLogicFormula)
  | Not (CTLLogicFormula)
  | ExistsNext (CTLLogicFormula)
  | ExistsPhiUntilPsi (CTLLogicFormula) (CTLLogicFormula)
  | ExistsAlwaysPhi (CTLLogicFormula)
    deriving (Eq, Show)

evaluateCTLFormula :: CTLLogicFormula -> Matrix m -> [Bool] -> Bool
evaluateCTLFormula (T) _ _ = True
evaluateCTLFormula (AP) _ _ = undefined
evaluateCTLFormula (And phi psi) m prior = (evaluateCTLFormula phi m prior) && (evaluateCTLFormula psi m prior)
evaluateCTLFormula (Not phi) m prior = not (evaluateCTLFormula phi m prior)
evaluateCTLFormula (ExistsNext phi) m prior = undefined --foldr (||) False (stepByFunc (evaluateCTLFormula phi) prior m (post)) 
evaluateCTLFormula (ExistsPhiUntilPsi phi psi) m prior = undefined -- defined later
evaluateCTLFormula (ExistsAlwaysPhi phi) m prior = undefined

satPsi :: [Bool]
satPsi = [False, False, True]

extendBy :: [Bool] -> (Matrix Bool -> Int -> [Bool]) -> Matrix Bool -> [Int]
extendBy prior step m = posterior
  where
    vertices = findIndices (id) prior
    vertices' = map (\x -> step m x) vertices
    posterior = nub $ [ vv | uu <- map (findIndices (id)) vertices', vv <- uu]

stepByFunc :: [Bool] -> [Bool] -> Matrix Bool -> (Matrix Bool -> Int -> [Bool]) -> [Bool]
stepByFunc [] _ _ _ = []
stepByFunc prior labelling m step = posterior
  where
    vertices  = extendBy prior step m
    reachable = filter (\x -> labelling !! x) vertices -- TODO: is there a more idiomatic way of doing this?
    posterior = map (\x -> labelling !! x) reachable

existsPhiUntilPsi :: Matrix Bool -> [Bool] -> [Bool] -> [Bool]
existsPhiUntilPsi matrix [] satisfy = satisfy
existsPhiUntilPsi matrix satPhi [] = []
existsPhiUntilPsi matrix satPhi satisfy =
  if satisfy' == satisfy
    then satisfy
    else existsPhiUntilPsi matrix satPhi satisfy'
  where
    satisfy' = nub $ satisfy `union` stepByFunc satisfy satPsi matrix (pre)

existsAlwaysPhi :: Matrix Bool -> [Bool] -> [Bool] -> [Bool]
existsAlwaysPhi matrix [] satisfy = []
existsAlwaysPhi matrix satPhi [] = []
existsAlwaysPhi matrix satPhi satisfy =
  if satisfy' == satisfy
    then satisfy
    else existsAlwaysPhi matrix satPhi satisfy'
  where
    satisfy' = nub $ satisfy `intersect` stepByFunc satisfy satPsi matrix (post)

main :: IO ()
main = do putStrLn "Compilation succeeded"
