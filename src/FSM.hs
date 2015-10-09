{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module FSM where

import qualified Data.Char as Char
import Control.Applicative
import qualified Data.Map as M
import Debug.Trace


data Tape a = Tape {leftTape :: [a], currentElement :: a, rightTape :: [a]}

initializeTape :: a -> Tape a
initializeTape a = let inf = (a:inf) in Tape inf a inf

read :: (Tape a) -> a
read (Tape _ a _) = a

write :: a -> (Tape a) -> (Tape a)
write a (Tape ls _ rs) = Tape ls a rs

transform :: (a -> a) -> Tape a -> Tape a
transform f t = write (f (FSM.read t)) t

moveRight :: Tape a -> Tape a
moveRight (Tape ls a (r:rs)) = Tape (a:ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) a rs) = Tape ls l (a:rs)

showTape :: (Show a) => Int -> Int -> Tape a -> String
showTape li ri (Tape ls a rs) = "..., " ++ (concat $ map ( (++", ").show ) $ reverse $ take li ls) ++ "{" ++ show a ++ "}, " ++ (concat $ map ( (++", ").show ) $ take ri rs) ++ "..."

instance (Show a) => Show (Tape a) where
    show = showTape 10 10

--tapeWhile :: (a -> Bool) -> (Tape a) -> [a]
--tapeWhile c (Tape ls a rs) = reverse (takeWhile c ls) ++ maybe a ++ (takeWhile c rs) where
--    maybe a 
--        | (c a) = [a]
--        | otherwise = []

data Transition a = Transition {condition :: a -> Bool, targetState :: State a}

data Action a = MoveLeft | MoveRight | Write a deriving Show
performAction :: Action a -> Tape a -> Tape a
performAction MoveLeft = moveLeft
performAction MoveRight = moveRight
performAction (Write a) = write a

data State a = State String [Action a] [Transition a] | HaltState

stateName :: State a -> String
stateName HaltState = "HaltState"
stateName (State n _ _) = n

stateTransitions :: State a -> [Transition a]
stateTransitions HaltState = []
stateTransitions (State _ _ ts) = ts


data TuringMachine a = TuringMachine {tape :: Tape a, state :: State a}

showTuringMachine :: (Show a) => TuringMachine a -> String
showTuringMachine (TuringMachine t HaltState) = "HaltState: "++show t
showTuringMachine (TuringMachine t (State n _ _)) = "STATE "++n++": "++show t

showTransition :: (Show a) => [a] -> Transition a -> String
showTransition values (Transition cond target) = show (filter (cond) values) ++ "->" ++ stateName target

showTransitions :: (Show a) => [a] -> [Transition a] -> [String]
showTransitions _ [] = []
showTransitions values (t@(Transition cond target):ts) = (showTransition values t) : showTransitions (filter (not . cond) values) ts

showState :: (Show a) => [a] -> State a -> String
showState _ HaltState = "HaltState"
showState values (State name actions transitions) = name++": do "++(show actions)++"; go: | "++ (concat $ map (++ " | ") $ showTransitions values transitions) 

reachableStates :: State a -> [State a]
reachableStates = graphWalk stateName (\ s -> map (targetState) (stateTransitions s))

class TryShow a where
  tryShow :: a -> String


uniq :: forall a e. (Eq e) => (a->e) -> [a] -> [a]
uniq eq = foldl (\acc a -> if ((eq a) `elem` (map eq acc)) then acc else (a:acc)) []

diff :: forall a e. (Eq e) => (a->e) -> [a] -> [a] -> [a]
diff eq big = foldl (\acc a -> if ((eq a) `elem` (map eq big)) then acc else (a:acc)) []

graphWalk :: forall n e. (Eq e) => (n -> e) -> (n -> [n]) -> n -> [n]
graphWalk eq next node = graphWalkRounds ([],[node]) where
    graphWalkRounds (branches,[]) = branches
    graphWalkRounds x = graphWalkRounds (graphWalkRound eq next x)

graphWalkRound :: forall n e. (Eq e) => (n -> e) -> (n -> [n]) -> ([n],[n]) -> ([n],[n])
graphWalkRound eq next (branches,leaves) =  (newBranches, newLeaves) where
    newBranches = branches ++ leaves
    newLeaves = diff eq newBranches $ uniq eq (concat $ map next leaves)

showFSM :: (Show a) => [a] -> State a -> [String]    
showFSM values start = map (showState values) (reachableStates start)


step :: TuringMachine a -> TuringMachine a

step tm@(TuringMachine t HaltState) = tm
step (TuringMachine t (State n as ts)) = TuringMachine nextTape (nextState ts) where
    nextTape = foldl (flip performAction) t as
    nextState [] = HaltState 
    nextState ((Transition c s):ts')     
        | (c (FSM.read nextTape)) == True = s
        | otherwise = nextState ts'


run :: TuringMachine a -> Tape a
run (TuringMachine t HaltState) = t
run tm = run (step tm)

scanrun :: TuringMachine a -> [TuringMachine a]    
scanrun tm@(TuringMachine t HaltState) = [tm]
scanrun tm = let next = step tm in tm : (scanrun next)

                 
makeFSM :: forall a. [(String, [Action a], [(a->Bool, String)])] -> State a
makeFSM x = let refs = (map buildFSM) x
                buildFSM :: (String, [Action a], [(a->Bool, String)]) -> State a
                buildFSM (na, ac, tr) = State na ac (map transform tr)                
                transform :: (a->Bool, String) -> Transition a
                transform (act, n) = Transition act (findname n refs)
                findname n [] = HaltState
                findname n (r : rs) 
                    | stateName r == n = r
                    | otherwise = findname n rs
                in refs !! 0

fsmAllOnes = makeFSM [("A", [MoveRight, Write 1], [(const True, "A")])]               

charToInt :: Char -> Int
charToInt c 
  | (Char.isDigit c) = Char.digitToInt c
  | otherwise = 0

fsmAddOne = makeFSM $ [
    ("Start", [], [(Char.isDigit, "Find Last Digit"),(const True, "Increment Digit")]),
    ("Find Last Digit", [MoveRight], [(Char.isDigit, "Find Last Digit"),(const True, "Left One Digit")]),
    ("Left One Digit", [MoveLeft], [(const True, "Increment Digit")]),
    ("Increment Digit", [], map (\c -> ((== c),"Inc"++[c])) ['1'..'9'] ++ [(const True, "Inc0")])
    ] ++ 
    map (\c -> ("Inc"++[c], [Write (Char.intToDigit(1+(Char.digitToInt c)))], [(const True, "Find First Digit")])) ['0'..'8']
    ++ [
    ("Inc9", [Write '0'], [(const True, "Left One Digit")]),
    ("Find First Digit", [MoveLeft], [(Char.isDigit, "Find First Digit"),(const True, "Right One Digit")]),
    ("Right One Digit", [MoveRight], [])
    ]

tapeToString (Tape ps c ns) = reverse (takeWhile (/= ' ') ps) ++ ['[',c,']'] ++ (takeWhile (/= ' ') ns)

stringToTape [] = initializeTape ' '
stringToTape (c:cs) = Tape (repeat ' ') c (cs ++ repeat ' ')


t0 = TuringMachine (stringToTape "352") fsmAddOne

tapes = iterate (\t -> run (TuringMachine t fsmAddOne)) (initializeTape ' ')


glonk = map (\(TuringMachine t s) -> (tapeToString t , (stateName s ) )   ) (scanrun (t0))
                
everyf n [] = []
everyf n as  = head as : everyf n (drop n as)

main = do
    print "start"
    --mapM_ putStrLn ((take 100) $ (everyf 37) $ map (tapeToString) tapes)
    --mapM_ (putStrLn . show) glonk    
    mapM_ (putStrLn) (showFSM (' ':['0'..'9']) fsmAddOne)
    print "done"