module LTL (module LTL) where

import Data.Graph (SCC(..), stronglyConnComp, flattenSCC)
import Data.List (elemIndex, findIndices, elemIndices, sort)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import Control.Parallel.Strategies

import TransitionSystem

data Buchi = Buchi {
    buchiTS :: [[Maybe String]]
  , accepting :: [Int] -- Accepting indices
  } deriving Show

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

-- | Get common elements between two lists of orderable components
getCommonElement :: Ord a => [a] -> [a] -> Bool
getCommonElement xs ys = not $ Set.null $ Set.intersection (Set.fromList xs) (Set.fromList ys)

-- | Get proposiitons holding in next Kripke state
getPropositionsPostKripke :: Kripke -> Int -> [String]
getPropositionsPostKripke k prior = concat propositions
  where
    posterior = elemIndices True $ post (kripkeTS k) prior
    propositions = mapMaybe (\state -> lookup state (kripkeLabel k)) posterior `using` parList rseq

-- | If state has no outgoing edges then add a self loop
stutterExtension :: [[Bool]] -> [[Bool]]
stutterExtension kripke = update kripke 0 indices
  where
    post = map (postInt kripke) [0..length kripke-1] `using` parList rseq
    indices = sort $ elemIndices [] post

    update k i [] = k
    update k i (x:xs) = update (replace k i x) (i+1) xs
    replace k i x = take x k ++ [take x (k !! x) ++ [True] ++ drop (x+1) (k !! x)] ++ drop (x+1) k

-- | Pad a matrix to be square
padToSquare :: [[Bool]] -> [[Bool]]
padToSquare m = padded'
  where
    maxLength = max (maximum $ map length m) (length m)

    padded = map (\row -> row ++ replicate (maxLength - length row) False) m `using` parList rseq
    padded' = map (\row -> row ++ replicate (maxLength - length row) False) padded `using` parList rseq

-- | Construct synchronous product of a Kripke structure and a Buchi automaton
synchronousProduct :: Kripke -> Buchi -> [[Bool]]
synchronousProduct kripke buchi = stutterExtension $ padToSquare $
  map
    (\s ->
      let
        propositions = getPropositionsPostKripke kripke s
      in
        map
          (\q ->
              let
               possibleBuchiTransitions = catMaybes $ post (buchiTS buchi) q
             in
               getCommonElement possibleBuchiTransitions propositions
          ) [0..length (buchiTS buchi) -1]
    ) [0..length (kripkeTS kripke)-1]

evaluateLTL :: Kripke -> Buchi -> (Bool, Maybe [Int])
evaluateLTL k b = (refuted, prefix)
  where
    syn = synchronousProduct k b
    bsccs = getBSCCs syn
    acceptingBsccs = filter (any (`elem` accepting b)) bsccs
    reachableStates = depthFirstSearch syn [] 0
  -- check if any reachable state is in an accepting BSCC
    refuted = not (any (any (`elem` reachableStates)) acceptingBsccs)
    prefix = if refuted then Just reachableStates else Nothing

adjMatrixToEdges :: [[Bool]] -> [(Int, Int, [Int])]
adjMatrixToEdges mat = [(i, i, adj i) | i <- nodes]
  where
    nodes = [0..length mat-1]
    adj i = [ j | (j, True) <- zip [0..] (mat !! i) ]

-- | Depth first search
depthFirstSearch :: [[Bool]] -> [Int] -> Int -> [Int]
depthFirstSearch matrix visited vertex
  | vertex `elem` visited = visited
  | otherwise = foldl (depthFirstSearch matrix) (vertex:visited) (reachableStates vertex) `using` parList rseq
  where
    reachableStates s = elemIndices True $ post matrix s
-- | Convert a adjacency matrix to a graph
adjMatrixToGraph :: [[Bool]] -> [(Int, Int, [Int])]
adjMatrixToGraph mat = [(i, i, adj i) | i <- nodes]
  where
    nodes = [0..length mat-1]
    adj i = [ j | (j, True) <- zip [0..] (mat !! i) ]

-- | Get the strongly connected components of a graph
getSCCs :: [[Bool]] -> [[Int]]
getSCCs m = map flattenSCC (stronglyConnComp $ adjMatrixToGraph m) `using` parList rseq

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
