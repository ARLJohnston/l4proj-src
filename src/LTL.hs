module LTL (module LTL) where

import Data.Graph (SCC(..), stronglyConnComp, flattenSCC)
import Data.List (elemIndex, findIndices, elemIndices)
import Control.Monad.State
import Control.Lens
import Debug.Trace

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

-- Buchi Automaton

-- | ASSUMES DETERMINISM
transitionByLabel :: [[Bool]] -> [Bool] -> Int -> Int
transitionByLabel ba l priorState =
    case elemIndex True reachableSatLabel of
      Nothing -> error "Buchi Automaton was not closed under transition" --This corresponds to the 'falling off case'
      Just posteriorState -> posteriorState
  where
    nextStates = ba !! priorState
    reachableSatLabel = zipWith (&&) l nextStates

-- | Given a state and list of labelling sets return which sets have that labelling
getStateLabelling :: Int -> [[Bool]] -> [Bool]
getStateLabelling state = map (!! state)
-- getStateLabelling state labellingSets = map (!! state) labellingSets


-- | Basic depth first search
depthFirstSearch :: [[Bool]] -> [Int] -> Int -> [Int]
depthFirstSearch matrix visited vertex
  | vertex `elem` visited = visited
  | otherwise = foldl (depthFirstSearch matrix) (vertex:visited) (reachableStates vertex)
  where
    reachableStates s = elemIndices True (matrix !! s)

{-
DFS from initial
Upon encountering an accepting state, begin nested DFS
If nestedDFS finds the state then there is a cycle
-}

-- | If length elemIndices index (dfs ..) > 1 => cycle
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
     if vertex `elem` accepting
       then
         if length (ndfs matrix [] vertex) > 1
           then
             ndfs matrix [] vertex
           else
             foldl (dfs matrix accepting) (vertex:visited) (reachableStates vertex)
       else
         foldl (dfs matrix accepting) (vertex:visited) (reachableStates vertex)
  where
    reachableStates s = elemIndices True (matrix !! s)

detectAcceptingCycles :: [[Bool]] -> [Int] -> Int -> Bool
detectAcceptingCycles matrix accepting initial = cycle
  where
    result = dfs matrix accepting [] initial
    cycle = length result > 1 && head result == result !! (length result -1)













adjMatrixToGraph :: [[Bool]] -> [(Int, Int, [Int])]
adjMatrixToGraph mat = [(i, i, adj i) | i <- nodes]
  where
    nodes = [0..length mat-1]
    adj i = [ j | (j, True) <- zip [0..] (mat !! i) ]

getSCCs :: [[Bool]] -> [[Int]]
getSCCs m = map flattenSCC (stronglyConnComp $ adjMatrixToGraph m)

data BuchiAutomaton = BuchiAutomaton {
  kripkeStructure :: [[Bool]],
  labelling :: [Bool],
  acceptingStates :: [Int]
  } deriving (Show)

