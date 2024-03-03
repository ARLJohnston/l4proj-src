module TransitionSystem (module TransitionSystem) where

import Data.List (findIndices)
import Control.Parallel.Strategies

-- | Get the states of the transition system which can transition to the given state.
pre :: [[a]] -> Int -> [a]
pre [] _ = []
pre adjacency column = map (!! column) adjacency `using` parList rseq

-- | Get the states of the transition system which the given state can transition to.
post :: [[a]] -> Int -> [a]
post [] _ = []
post adjacency row = adjacency !! row

-- | Given a prior set of vertices, satisfaction set, transition system and transitory function, return the vertices which can be reached from the vertices where the satisfaction is True.
stepByFunc :: [Bool] -> [Bool] -> [[Bool]]-> ([[Bool]]-> Int -> [Bool]) -> [Bool]
stepByFunc [] _ _ _ = []
stepByFunc prior labelling m step = posterior
  where
--States we can reach
    vertices = extendBy prior step m
--Filter to states where the predicate is true
    reachable = filter (labelling !!) vertices
    posterior = map (`elem` reachable) [0..length prior - 1]

-- | Given a prior set of vertices, transition system and transitory function, return the vertices reachable from the vertices via the transitory function.
getReachableByFunc :: [Bool] -> [[Bool]] -> ([[Bool]] -> Int -> [Bool]) -> [Bool]
getReachableByFunc [] _ _ = []
getReachableByFunc prior m step = posterior
  where
    reachable = extendBy prior step m
    posterior = map (`elem` reachable) [0..length prior - 1]

-- | Given a satisfaction set, transitory function and transition system, return the indices which can be reached from True states in the satisfaction set via the transitory function.
extendBy :: [Bool] -> ([[Bool]] -> Int -> [Bool]) -> [[Bool]] -> [Int]
extendBy prior step m = posterior
  where
    vertices = findIndices id prior
    vertices' = map (step m) vertices `using` parList rseq
    posterior = [ vv | uu <- map (findIndices id) vertices', vv <- uu]
