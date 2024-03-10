module LTL (module LTL) where

import Data.Graph (SCC(..), stronglyConnComp, flattenSCC)
import Data.List (elemIndex, findIndices, elemIndices, find)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import Control.Parallel.Strategies
import Debug.Trace

import TransitionSystem

data Buchi = Buchi {
    buchiTS :: [[Maybe Char]]
  , accepting :: [Int] -- Accepting indices
  } deriving Show


getCommonElement :: Ord a => [a] -> [a] -> Bool
getCommonElement xs ys = not $ Set.null $ Set.intersection (Set.fromList xs) (Set.fromList ys)

getPropositionsPostKripke :: Kripke -> Int -> [Char]
getPropositionsPostKripke k prior = concat propositions
  where
    posterior = elemIndices True $ post (kripkeTS k) prior
    propositions = mapMaybe (\state -> lookup state (kripkeLabel k)) posterior

synchronousProduct :: Kripke -> Buchi -> [[Bool]]
synchronousProduct kripke buchi =
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
    reachableBsccs = depthFirstSearch syn [] 0
    refuted = any (\bscc -> any (`elem` accepting b) bscc && any (`elem` bscc) reachableBsccs) bsccs
    prefix = if refuted then Just reachableBsccs else Nothing

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
    reachableStates s = elemIndices True (matrix !! s)
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

-- evaluateLTL :: Eq a => [[Bool]] -> BuchiState a -> [[a]] -> (Bool, Maybe [Int])
-- evaluateLTL transitionSystem buchi labelling = (refuted, prefix)
--   where
--     syn = synchronousProduct transitionSystem labelling buchi
--     initial = findIndices initial buchi
--     accepting = findIndices accepting buchi
--     bsccs = getBSCCs syn
--     reachableBsccs = depthFirstSearch syn [] initial
--     refuted = any (\bscc -> any (`elem` labelling) bscc && any (`elem` bscc) reachableBsccs) bsccs
--     prefix = if refuted then Just reachableBsccs else Nothing
-- 
-- generateBuchiStates :: [BuchiState Char]
-- generateBuchiStates = state0 : [state1]
--   where
--     state0 = BuchiState {
--       transitions = [('b', state0),('a', state1)],
--       accepting = False,
--       initial = True,
--       index = 0
--   }
--     state1 = BuchiState {
--         transitions = [('a', state1)],
--         accepting = True,
--          initial = False,
--         index = 1
--     }
