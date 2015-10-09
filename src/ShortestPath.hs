module ShortestPath (shortestPath) where

import Data.List

import qualified Data.Set as Set

on :: (c -> c -> b) -> (a -> c) -> (a -> a -> b)
on f g x y = f (g x) (g y)

eqOn :: (Eq c) => (a -> c) -> (a -> a -> Bool)
eqOn = on (==)

extendPaths :: (Ord s) => (s -> [s]) -> ([[s]],Set.Set s) -> ([[s]],Set.Set s)
extendPaths next (paths, prevs) = (nubBy (eqOn head) nextPaths, nextPrevs) where
    next' = filter (`Set.notMember` prevs) . next 
    nextPrevs = Set.union (Set.fromList (map head nextPaths)) prevs
    nextPaths = concatMap extendPath paths where       
    extendPath [] = []
    extendPath path = map (:path) (next' (head path))

allElems :: (Ord s, Eq s) => [[s]] -> Set.Set s
allElems sss = Set.fromList (concat sss)

limitedIterate :: (a -> Maybe a) -> a -> [a]
limitedIterate f a = case (f a) of
    (Just b) -> a : limitedIterate f b
    Nothing -> [a]

allPaths :: (Ord s, Eq s) => (s -> [s]) -> [[s]] -> [[s]]
allPaths next paths = concatMap fst $ limitedIterate extend (paths, allElems paths) where
    extend ps = case (extendPaths next ps) of
        ([],_) -> Nothing
        x -> Just x

allPathsStartingFrom :: (Ord s, Eq s) => (s -> [s]) -> s -> [[s]]
allPathsStartingFrom f s = allPaths f [[s]]

-- shortestPath takes a function representing a directed graph 
--  (a function take takes a state and returns all the states directly accessible from it)
--  and a predicate for determining the end state
--  and a starting state
--  and returns the shortest path from the starting state to the end state (tie breaker?)
shortestPath :: (Ord s) => (s -> [s]) -> (s -> Bool) -> s -> [s]
shortestPath next endPred start = case (find (endPred . head) (allPathsStartingFrom next start)) of
    Nothing -> []
    Just p -> reverse p


seeExplosion :: (Ord s) => (s -> [s]) -> s -> [Int]
seeExplosion next start = map (Set.size . snd) $ iterate (extendPaths next) ([[start]],Set.singleton start)

    
