module CTL (module CTL) where

import Data.Matrix (Matrix, getCol, getRow, nrows)
import Data.Vector (toList)
import Data.List (findIndices)

import Control.Parallel.Strategies

-- | Get the states of the transition system which can transition to the given state.
pre :: Matrix a -> Int -> [a]
pre m n = toList $ getCol (n+1) m

-- | Get the states of the transition system which the given state can transition to.
post :: Matrix a -> Int -> [a] 
post m n = toList $ getRow (n+1) m

-- | Given a prior set of vertices, satisfaction set, transition system and transitory function, return the vertices which can be reached from the vertices where the satisfaction is True.
stepByFunc :: [Bool] -> [Bool] -> Matrix Bool -> (Matrix Bool -> Int -> [Bool]) -> [Bool]
stepByFunc [] _ _ _ = []
stepByFunc prior labelling m step = posterior
  where
--States we can reach
    vertices = extendBy prior step m
--Filter to states where the predicate is true
    reachable = filter (labelling !!) vertices
    posterior = map (`elem` reachable) [0..length prior - 1]

-- | Given a prior set of vertices, transition system and transitory function, return the vertices reachable from the vertices via the transitory function.
getReachableByFunc :: [Bool] -> Matrix Bool -> (Matrix Bool -> Int -> [Bool]) -> [Bool]
getReachableByFunc [] _ _ = []
getReachableByFunc prior m step = posterior
  where
    reachable = extendBy prior step m
    posterior = map (`elem` reachable) [0..length prior - 1]

-- | Given a satisfaction set, transitory function and transition system, return the indices which can be reached from True states in the satisfaction set via the transitory function.
extendBy :: [Bool] -> (Matrix Bool -> Int -> [Bool]) -> Matrix Bool -> [Int]
extendBy prior step m = posterior
  where
    vertices = findIndices id prior
    vertices' = map (step m) vertices `using` parList rseq
    posterior = [ vv | uu <- map (findIndices id) vertices', vv <- uu]

-- | Recursive Data Structure representing a CTLFormula.
data CTLFormula =
    Satisfaction [Bool]
  | Atomic [Bool]
  | And CTLFormula CTLFormula
  | Or CTLFormula CTLFormula
  | Not CTLFormula
  | ExistsNext CTLFormula
  | ExistsPhiUntilPsi CTLFormula CTLFormula
  | ExistsAlways CTLFormula
  | ExistsEventually CTLFormula
  | ForAllNext CTLFormula
  | ForAllPhiUntilPsi CTLFormula CTLFormula
  | ForAllEventually CTLFormula
  | ForAllAlways CTLFormula
    deriving (Eq)

instance Show CTLFormula where
  show (Satisfaction satisfy) = "Sat(" ++ show satisfy ++ ")"
  show (Atomic satisfy) = "Sat(" ++ show satisfy ++ ")"
  show (And phi psi) = "(" ++ show phi ++ ") ^ (" ++ show psi ++ ")"
  show (Or phi psi) = "(" ++ show phi ++ ") v (" ++ show psi ++ ")"
  show (Not phi) = "¬(" ++ show phi ++ ")"
  show (ExistsNext phi) = "∃X(" ++ show phi ++ ")" 
  show (ExistsPhiUntilPsi phi psi) = "∃((" ++ show phi ++ ") U (" ++ show psi ++ "))"
  show (ExistsAlways phi) = "∃☐(" ++ show phi ++ ")"
  show (ForAllNext phi) = "∀X(" ++ show phi ++ ")"
  show (ForAllPhiUntilPsi phi psi) = "∀((" ++ show phi ++ ") U (" ++ show psi ++ "))"
  show (ForAllEventually phi) = "∀◇(" ++ show phi ++ ")"
  show (ForAllAlways phi) = "∀☐(" ++ show phi ++ ")"
  show (ExistsEventually phi) = "∃◇(" ++ show phi ++ ")"

-- |  Evaluate a 'CTLFormula' on a given transition system.
--
-- > transitionSystem :: Matrix Bool
-- > transitionSystem = fromLists [[True, False],[True, True]]
-- >
-- > formula :: CTLFormula
-- > formula = And (Satisfaction [True, True]) (Satisfaction [False, True])
-- >
-- > evaluateCTL formula transitionSystem = [False, True]
evaluateCTL :: CTLFormula -> Matrix Bool -> [Bool]
evaluateCTL (Satisfaction satisfy) _ = satisfy

evaluateCTL (Atomic satisfy) _ = satisfy

evaluateCTL (And phi psi) m = zipWith (&&) (evaluateCTL phi m) (evaluateCTL psi m) `using` parList rseq

evaluateCTL (Or phi psi) m = zipWith (||) (evaluateCTL phi m) (evaluateCTL psi m) `using` parList rseq

evaluateCTL (Not phi) m = map not (evaluateCTL phi m) `using` parList rseq

evaluateCTL (ExistsNext phi) m = lastPhi
  where
    satisfy = evaluateCTL phi m
    lastPhi = getReachableByFunc satisfy m pre `using` parList rseq

evaluateCTL (ExistsPhiUntilPsi phi psi) m = existsPhiUntilPsi m (evaluateCTL phi m) (evaluateCTL psi m)
evaluateCTL (ExistsAlways phi) m = existsAlwaysPhi m (evaluateCTL phi m)

evaluateCTL (ForAllNext phi) m = map not (lastNotPhi) `using` parList rseq
  where
    notPhi = map not (evaluateCTL phi m) `using` parList rseq
    lastNotPhi = getReachableByFunc notPhi m pre `using` parList rseq

evaluateCTL (ForAllPhiUntilPsi phi psi) m = zipWith (&&) notPhiUntilNotPhiAndPsi doesNotExistNotPsi `using` parList rseq
  where 
    notPhi = map not (evaluateCTL phi m) `using` parList rseq
    notPsi = map not (evaluateCTL psi m) `using` parList rseq
    notPhiAndNotPsi = zipWith (&&) notPhi notPsi `using` parList rseq
    doesNotExistNotPsi = map not (existsAlwaysPhi m notPsi) `using` parList rseq
    notPhiUntilNotPhiAndPsi = map not (existsPhiUntilPsi m notPsi notPhiAndNotPsi) `using` parList rseq

evaluateCTL (ForAllEventually phi) m = map not (existsAlwaysPhi m notPhi) `using` parList rseq
  where
    notPhi = map not (evaluateCTL phi m) `using` parList rseq

evaluateCTL (ForAllAlways phi) m = map not (existsPhiUntilPsi m true notPhi) `using` parList rseq
  where
    notPhi = map not (evaluateCTL phi m) `using` parList rseq
    true = replicate (nrows m) True 

evaluateCTL (ExistsEventually phi) m = map not alwaysNotPhi `using` parList rseq
  where
    notPhi = map not (evaluateCTL phi m) `using` parList rseq
    forAllAlwaysNotPhi = ForAllAlways (Satisfaction notPhi)
    alwaysNotPhi = evaluateCTL forAllAlwaysNotPhi m



-- | Return the states where ∃ΦUΨ holds.
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

-- | Return the states where ∃☐Φ holds.
existsAlwaysPhi :: Matrix Bool -> [Bool] -> [Bool]
existsAlwaysPhi _ [] = []
existsAlwaysPhi matrix satisfy =
  if satisfy' == satisfy
    then satisfy
    else existsAlwaysPhi matrix satisfy'
  where
    nextStep = stepByFunc satisfy satisfy matrix pre
    satisfy' = zipWith (&&) satisfy nextStep `using` parList rseq
