module CTL (module CTL) where

import Control.Parallel.Strategies

import TransitionSystem

-- | Recursive Data Structure representing a CTLFormula.
data CTLFormula =
    CTLLabel [Bool]
  | CTLAtom [Bool]
  | CTLAnd CTLFormula CTLFormula
  | Or CTLFormula CTLFormula
  | CTLNot CTLFormula
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
  show (CTLLabel satisfy) = "Sat(" ++ show satisfy ++ ")"
  show (CTLAtom satisfy) = "Sat(" ++ show satisfy ++ ")"
  show (CTLAnd phi psi) = "(" ++ show phi ++ ") ^ (" ++ show psi ++ ")"
  show (Or phi psi) = "(" ++ show phi ++ ") v (" ++ show psi ++ ")"
  show (CTLNot phi) = "¬(" ++ show phi ++ ")"
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
-- > transitionSystem :: [[Bool]]
-- > transitionSystem = [[True, False],[True, True]]
-- >
-- > formula :: CTLFormula
-- > formula = CTLAnd (CTLLabel [True, True]) (CTLLabel [False, True])
-- >
-- > evaluateCTL formula transitionSystem = [False, True]
evaluateCTL :: CTLFormula -> [[Bool]]-> [Bool]
evaluateCTL (CTLLabel satisfy) _ = satisfy

evaluateCTL (CTLAtom satisfy) _ = satisfy

evaluateCTL (CTLAnd phi psi) m = zipWith (&&) (evaluateCTL phi m) (evaluateCTL psi m) `using` parList rseq

evaluateCTL (Or phi psi) m = zipWith (||) (evaluateCTL phi m) (evaluateCTL psi m) `using` parList rseq

evaluateCTL (CTLNot phi) m = map not (evaluateCTL phi m) `using` parList rseq

evaluateCTL (ExistsNext phi) m = lastPhi
  where
    satisfy = evaluateCTL phi m
    lastPhi = getReachableByFunc satisfy m pre `using` parList rseq

evaluateCTL (ExistsPhiUntilPsi phi psi) m = existsPhiUntilPsi m (evaluateCTL phi m) (evaluateCTL psi m)
evaluateCTL (ExistsAlways phi) m = existsAlwaysPhi m (evaluateCTL phi m)

evaluateCTL (ForAllNext phi) m = map not lastNotPhi `using` parList rseq
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
    true = replicate (length m) True

evaluateCTL (ExistsEventually phi) m = map not alwaysNotPhi `using` parList rseq
  where
    notPhi = map not (evaluateCTL phi m) `using` parList rseq
    forAllAlwaysNotPhi = ForAllAlways (CTLLabel notPhi)
    alwaysNotPhi = evaluateCTL forAllAlwaysNotPhi m

-- | Return the states where ∃ΦUΨ holds.
existsPhiUntilPsi :: [[Bool]] -> [Bool] -> [Bool] -> [Bool]
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
existsAlwaysPhi :: [[Bool]] -> [Bool] -> [Bool]
existsAlwaysPhi _ [] = []
existsAlwaysPhi matrix satisfy =
  if satisfy' == satisfy
    then satisfy
    else existsAlwaysPhi matrix satisfy'
  where
    nextStep = stepByFunc satisfy satisfy matrix pre
    satisfy' = zipWith (&&) satisfy nextStep `using` parList rseq
