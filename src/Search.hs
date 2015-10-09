{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Search where

import Data.List

import Prelude hiding (Left,Right)
import qualified Prelude as P

import qualified Data.Map.Strict as Map
import System.Random

on :: (c -> c -> b) -> (a -> c) -> (a -> a -> b)
on f g x y = f (g x) (g y)

compareBy :: (Ord b) => (a -> b) -> (a -> a -> Ordering)
compareBy = (compare `on`)

data Reverse a = Reverse {unreverse :: a}

instance (Eq a) => Eq (Reverse a) where
    (Reverse a) == (Reverse b) = a == b
instance (Ord a) => Ord (Reverse a) where
    compare (Reverse a) (Reverse b) = compare b a

maxIndexBy :: (a -> a -> Ordering) -> [a] -> Int
maxIndexBy compare as = snd $ maximumBy (compare `on` fst) (zip as [0..])


{--
chooseBestMove :: (Ord f) => (s -> f) -> (s -> m -> s) -> s -> [m] -> Maybe m
chooseBestMove fitnessFunction stateUpdateFunction startState possibleMoves = bestMove where
    nextStates = map (stateUpdateFunction startState) possibleMoves
    fitnesses = map (fitnessFunction) nextStates
    (bestMove, _) = maximumBy (compare `on` snd) ((Nothing, Nothing) : zip (map Just possibleMoves) (map Just fitnesses))

--expectedFitness :: (Num f, Num g) => (s -> f) -> (s -> [m]) -> (m -> s -> s) -> (m -> g) -> s -> f
--expectedFitness fitnessFunction possibleMoveFunction stateUpdateFunction probabilityOfMoves 

expectedValue :: (Fractional y, p ~ y) => (x -> y) -> [(x,p)] -> y
expectedValue f statesAndProbs = sum (map (\(x,p) -> f x * p) statesAndProbs) / sum (map snd statesAndProbs)

expectedFitness :: (Fractional f, p ~ f) => (s -> f) ->  (s -> m -> s) -> s -> [(m,p)] -> f
expectedFitness baseFitness  stateUpdateFunction startState possibleMovesAndProbs = fitnessval where     
    possibleStatesAndProbs = map (\(m,p) -> (stateUpdateFunction startState m, p)) possibleMovesAndProbs
    fitnessval = if (null possibleMovesAndProbs) then (baseFitness startState) else expectedValue baseFitness possibleStatesAndProbs 

--}


{--
chooseWorstMove :: (Ord f) => (s -> f) -> (s -> m -> s) -> s -> [m] -> Maybe m
chooseWorstMove fitnessFunction = chooseBestMove (Reverse . fitnessFunction) 

opponentPicksWorst :: (Ord f, Num p, p ~ f) => (s -> f) -> (s -> m -> s) -> s -> [m] -> [(m,p)]
opponentPicksWorst _ _ _ [] = []
opponentPicksWorst fitnessFunction stateUpdateFunction startState possibleMoves = possibleMovesAndProbs where    
    fitnesses = map (fitnessFunction . (stateUpdateFunction startState)) possibleMoves  
    choice = maxIndexBy (compare `on` Reverse) fitnesses
    probs = map (\i -> (if i==choice then 1 else 0)) [0..]
    possibleMovesAndProbs = zip possibleMoves probs

updateFitness :: (Fractional f, Ord f) => (s -> m -> s) -> (s -> [m]) -> (s -> f) -> (s -> f)
updateFitness stateUpdateFunction possibleMoveFunction baseFitness startState = uf where
    uf = expectedFitness baseFitness stateUpdateFunction startState (opponentPicksWorst baseFitness stateUpdateFunction startState (possibleMoveFunction startState))
    
-- whoops maybe not


-- start over

--}

type FitnessFunction g = g -> Double
type Probability = Double
type ProbabilityAssigner g = g -> Move g -> Probability

class GameState g where    
    type Move g
    possibleMoves :: g -> [Move g]
    nextState :: g -> Move g -> g
    initState :: g
    --baseFitness :: FitnessFunction g
{--
lookAheadFitness :: (GameState g) => ProbabilityAssigner g -> FitnessFunction g -> FitnessFunction g
lookAheadFitness prob fitness = lookaheadZeroFitness where
    lookaheadZeroFitness startingState = sum weightedFitnesses / sum weights where
        nextMoves = possibleMoves startingState
        weights = map (prob startingState) nextMoves
        fitnesses = map (fitness . nextState startingState) nextMoves
        weightedFitnesses = map (uncurry (*)) (zip weights fitnesses)
--}

-- this function will create a new fitness function from an old one.  The new one assigns a new fitness to state s
-- that is the minimum of all the old fitnesses for all states one move beyond s. The idea here is that if you treat
-- s as a state where your opponent moves, you assume your opponent will always choose the worst move for you.  The
-- only difference: if there are no moves leading from s then the new fitness will just be the old fitness

opponentsMoveFitness :: (GameState g) => FitnessFunction g -> FitnessFunction g
opponentsMoveFitness fitness = minNextMoveFitness where
    minNextMoveFitness startingState = if (null nextMoves) then fitness startingState else worstFitness where        
        nextMoves = possibleMoves startingState
        worstFitness = minimum $ map (fitness . nextState startingState) nextMoves
        
-- this function will create a new fitness function from an old one.  The new one assigns a new fitness to state s
-- that is the maximum of all the old fitnesses for all states one move beyond s.  The idea is if you treat s
-- as a state where you move, you want to choose the one which is best for you.  Again, if there are no moves leading from s, 
-- the new fitness is just the old one.

myMoveFitness:: (GameState g) => FitnessFunction g -> FitnessFunction g
myMoveFitness fitness = maxNextMoveFitness where    
    maxNextMoveFitness startingState = if (null nextMoves) then fitness startingState else bestFitness where
        nextMoves = possibleMoves startingState
        bestFitness = maximum $ map (fitness . nextState startingState) nextMoves

lookAheadFitness :: (GameState g) => FitnessFunction g -> FitnessFunction g
lookAheadFitness = myMoveFitness . opponentsMoveFitness

lookAheadFitnessN :: (GameState g) => FitnessFunction g -> Int -> FitnessFunction g
lookAheadFitnessN f i = iterate lookAheadFitness f !! i

data Mark = X | O deriving (Enum, Bounded, Ord, Eq, Show)
data Box = UpperLeft | Upper | UpperRight | Left | Center | Right | LowerLeft | Lower | LowerRight deriving (Enum, Bounded, Ord, Eq, Show)
data TicTacToe = TicTacToe (Map.Map Box Mark) deriving Show

winningBoxSets = [[UpperLeft, Upper, UpperRight],[Left,Center,Right],[LowerLeft,Lower,LowerRight],
    [UpperLeft,Left,LowerLeft],[Upper, Center, Lower],[UpperRight, Right, LowerRight],
    [UpperLeft,Center,LowerRight],[UpperRight,Center,LowerLeft]]

whoseTurn :: TicTacToe -> Mark
whoseTurn (TicTacToe m) = if (diff <= 0) then X else O where
    diff = Map.foldr cnt 0 m
    cnt X = (+1)
    cnt O = (`subtract` 1)

emptyBoxes :: TicTacToe -> [Box]
emptyBoxes (TicTacToe m) = filter (flip Map.notMember m) [minBound..maxBound]

hasWon :: Mark -> TicTacToe -> Bool
hasWon x (TicTacToe m) = any (hasAllSquares) winningBoxSets where
    hasAllSquares bs = all (\b -> Just x == Map.lookup b m) bs
    

instance GameState TicTacToe where
    type Move TicTacToe = (Box, Mark)
    possibleMoves t = if (hasWon X t || hasWon O t) then [] else map (\b -> (b, whoseTurn t)) (emptyBoxes t)
    nextState (TicTacToe m) (box, mark) = TicTacToe (Map.insert box mark m)
    initState = TicTacToe Map.empty



baseFitness :: TicTacToe -> Double
baseFitness t 
    | hasWon X t = 1
    | hasWon O t = -1
    | otherwise = 0

okayFitness = lookAheadFitnessN baseFitness 3

i = initState::TicTacToe

main = do    
    print "Search"
    print $ map (\x -> (x, okayFitness x) ) $ map (nextState i) (possibleMoves i)
    print "done"