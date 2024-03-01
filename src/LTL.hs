module LTL (module LTL) where

import Data.Graph (SCC(..), stronglyConnComp, flattenSCC)
import Data.List (elemIndex, findIndices, elemIndices)
import qualified Data.Set as Set

import CTL

data LTLFormula =
    LTLLabel [Bool]
  | LTLAtom [Bool]
  | LTLAnd LTLFormula LTLFormula
  | LTLNot LTLFormula
  | Next LTLFormula
  | Until LTLFormula LTLFormula
    deriving (Eq)

instance Show LTLFormula where
  show (LTLLabel satisfy) = "Sat(" ++ show satisfy ++ ")"
  show (LTLAtom satisfy) = "Sat(" ++ show satisfy ++ ")"
  show (LTLAnd phi psi) = "(" ++ show phi ++ ") ^ (" ++ show psi ++ ")"
  show (LTLNot phi) = "¬(" ++ show phi ++ ")"
  show (Next phi) = "X(" ++ show phi ++ ")"
  show (Until phi psi) = "(" ++ show phi ++ ") U (" ++ show psi ++ ")"


-- | Given a state in a Buchi automaton, return the state which satisfy one of the labels and are reachable from the state given or Nothing if no such state exists. This method assumes the automaton is deterministic.
transitionByLabel :: [[Bool]] -> [Bool] -> Int -> Maybe Int
transitionByLabel ba l priorState = elemIndex True reachableSatLabel
  where
    nextStates = ba !! priorState
    reachableSatLabel = zipWith (&&) l nextStates


-- | Given a Kripke structure and Buchi automaton, create the synchronous product
createSynchronousProduct :: [[Bool]] -> [[Bool]] -> [Int] -> [[Bool]]
createSynchronousProduct kripke buchi labelling = syn
  where
    syn = map (
      \kripkeState -> map (\index ->
        case transitionByLabel buchi (buchi !! index) kripkeState of
          Nothing -> False
          Just _ -> True
        ) [0..length buchi-1]
      ) [0..length kripke-1]
{-
for s in kripke:
  for q in buchi:
    for state in post(s)

      case transition q by labelling(t) of
        Just _ -> (s,q) -> (t, q')
        Nothing -> No transition

Resulting size is length kripke * length buchi
Accepting is the columns of previous accepting
-}

-- | Given a Kripke structure and a Buchi automaton, determine if the formula is refuted
evaluateLTL :: [[Bool]] -> [[Bool]] -> [Int] -> Int -> (Bool, Maybe [Int])
evaluateLTL transitionSystem buchi labelling initial = (refuted, prefix)
  where
    syn = createSynchronousProduct transitionSystem buchi labelling
    bsccs = getBSCCs syn
  --get reachable BSCCs from initial state
    reachableBsccs = depthFirstSearch syn [] initial
    refuted = any (\bscc -> any (`elem` labelling) bscc && any (`elem` bscc) reachableBsccs) bsccs
    prefix = if refuted then Just reachableBsccs else Nothing

-- | Depth first search
depthFirstSearch :: [[Bool]] -> [Int] -> Int -> [Int]
depthFirstSearch matrix visited vertex
  | vertex `elem` visited = visited
  | otherwise = foldl (depthFirstSearch matrix) (vertex:visited) (reachableStates vertex)
  where
    reachableStates s = elemIndices True (matrix !! s)

-- Worse approach to computing satisfiability
ndfs :: [[Bool]] -> [Int] -> Int -> [Int]
ndfs matrix visited vertex
  | vertex `elem` visited = vertex:visited
  | otherwise = foldl (ndfs matrix) (vertex:visited) (reachableStates vertex)
  where
    reachableStates s = elemIndices True (matrix !! s)

-- | Depth first search that runs ndfs if vertex is in accepting
dfs :: [[Bool]] -> [Int] -> [Int] -> Int -> [Int]
dfs matrix accepting visited vertex
  | vertex `elem` visited = visited
  | otherwise =
     if vertex `elem` accepting && length (ndfs matrix [] vertex) > 1 then ndfs matrix [] vertex else foldl (dfs matrix accepting) (vertex:visited) (reachableStates vertex)
  where
    reachableStates s = elemIndices True (matrix !! s)

-- | Given a Kripke structure and a Buchi automaton, get if an accepting cycle exists
detectAcceptingCycles :: [[Bool]] -> [Int] -> Int -> Bool
detectAcceptingCycles matrix accepting initial = cycle
  where
    result = dfs matrix accepting [] initial
    cycle = length result > 1 && head result == result !! (length result -1)

-- | Convert a adjacency matrix to a graph
adjMatrixToGraph :: [[Bool]] -> [(Int, Int, [Int])]
adjMatrixToGraph mat = [(i, i, adj i) | i <- nodes]
  where
    nodes = [0..length mat-1]
    adj i = [ j | (j, True) <- zip [0..] (mat !! i) ]

-- | Get the strongly connected components of a graph
getSCCs :: [[Bool]] -> [[Int]]
getSCCs m = map flattenSCC (stronglyConnComp $ adjMatrixToGraph m)

-- | Get the post states of a state as [Int]
postInt :: [[Bool]] -> Int -> [Int]
postInt m i = elemIndices True (m !! i)

-- | Get the post states of a list of states as [Int]
postListInt :: [[Bool]] -> [Int] -> [Int]
postListInt m = concatMap (postInt m)

-- | Check if a list of states is closed under the transition relation
closed :: [Int] -> [[Bool]] -> Bool
closed x m = Set.fromList x == Set.fromList (postListInt m x)

-- | Get the bottom/terminal strongly connected components of a graph
getBSCCs :: [[Bool]] -> [[Int]]
getBSCCs m = filter (`closed` m) sccs
  where
    sccs = getSCCs m
