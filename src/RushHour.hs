{-# LANGUAGE TupleSections #-}
module RushHour where

import Data.List

import qualified Data.Set as Set
import qualified Data.Map as Map

import ShortestPath


data Vehicle = Car | Truck deriving (Eq, Ord, Show)

vehicleLength :: Vehicle -> Int
vehicleLength Car = 2
vehicleLength Truck = 3

data Orientation = Horizontal | Vertical deriving (Eq, Ord, Show)

type Position = (Int, Int)

type PositionedVehicle = (Vehicle, Position, Orientation)

data Board = Board Int [PositionedVehicle] deriving (Eq, Ord, Show)

translate :: Orientation -> Int -> Position -> Position
translate Horizontal d (x,y) = (x+d, y)
translate Vertical d (x,y) = (x,y+d)

allPositions :: PositionedVehicle -> Set.Set Position
allPositions (v, p, o) = Set.fromList $ take (vehicleLength v) $ iterate (translate o 1) p where

pairs [] = []
pairs (a:as) = map (a,) as ++ pairs as

setAny :: (Ord a) => (a -> Bool) -> (Set.Set a) -> Bool
setAny pred = not . Set.null . Set.filter pred

setAll :: (Ord a) => (a -> Bool) -> (Set.Set a) -> Bool
setAll pred = Set.null . Set.filter (not . pred)

validBoard :: Board -> Bool 
validBoard (Board s pvs) = allVehiclesOnBoard && noVehiclesIntersect where
    allVehiclesOnBoard = all onBoard pvs
    onBoard pv = setAll legalPositions (allPositions pv)
    legalPositions = (\(x,y) -> x>=0 && y>=0 && x<s && y<s)
    noVehiclesIntersect = all noIntersect (pairs pvs)
    
noIntersect :: (PositionedVehicle, PositionedVehicle) -> Bool   
noIntersect ((_, (_,y), Horizontal),(_,(_,y'), Horizontal))
 | (y/=y') = True
noIntersect ((v, (x,_), Horizontal),(v',(x',_), Horizontal))
 | x + (vehicleLength v) <= x' = True
 | x'+ (vehicleLength v') <= x = True
 | otherwise = False
noIntersect ((_, (x,_), Vertical),(_, (x',_), Vertical))
 | (x/=x') = True
noIntersect ((v, (_,y), Vertical),(v',(_,y'), Vertical))
 | y + (vehicleLength v) <= y' = True
 | y'+ (vehicleLength v') <= y = True
 | otherwise = False
noIntersect (pv1,pv2) = Set.null $ Set.intersection (allPositions pv1) (allPositions pv2)

        
maybeBoard :: Board -> Maybe Board
maybeBoard b = if (validBoard b) then Just b else Nothing

adjust :: Int -> (a -> a) -> [a] -> [a]
adjust i f as = take i as ++ ((f (as !! i)) : drop (i+1) as)

moveCar :: Int -> Int -> Board -> Maybe Board
moveCar i x (Board s pvs) = maybeBoard (Board s (adjust i f pvs)) where
    f (v, p, o) = (v, (translate o x p), o)

deMaybe :: [Maybe a] -> [a]
deMaybe [] = []
deMaybe (Nothing:as) = deMaybe as
deMaybe (Just a : as) = a : deMaybe as

allMovesForOneCar :: Board -> Int -> [Board]
allMovesForOneCar b i = deMaybe [moveCar i (-1) b, moveCar i (1) b]
--reverse (tail $ limitedIterate (moveCar i (-1)) b) ++ (tail $ limitedIterate (moveCar i 1) b)

allMovesForAllCars :: Board -> [Board]    
allMovesForAllCars b@(Board s pvs) = concatMap (allMovesForOneCar b) [0..length pvs - 1]

isSolved :: Board -> Bool
isSolved (Board s pvs) = any isCarInRightPlace pvs where
    isCarInRightPlace (Truck, _, _) = False
    isCarInRightPlace (_, _, Vertical) = False
    isCarInRightPlace (v, p, Horizontal) = p == (s-(vehicleLength v), 2)

solveBoard :: Board -> [Board]    
solveBoard = shortestPath allMovesForAllCars isSolved




showBoard' :: Board -> String
showBoard' (Board s pvs) = [ if (i==s) then '\n' else carSymbol (i,j) | j <- [0..s-1], i<-[0..s]] where
    carSymbol p = case (find ((Set.member p) . allPositions . fst)) (zip pvs ['A'..]) of
        Nothing -> '.'
        (Just (_,c)) -> c

add :: Position -> Position -> Position
add (x,y) (a,b) = (x+a,y+b)

r = add (1,0)   
d = add (0,1)
    
putVehicleOnMap :: (PositionedVehicle, Char) -> Map.Map Position [Char] -> Map.Map Position [Char]
putVehicleOnMap ((Car, p, Horizontal), c) m = foldr (uncurry Map.insert) m [(p,"[═"),(r p, "═]")]
putVehicleOnMap ((Truck, p, Horizontal), c) m = foldr (uncurry Map.insert) m [(p,"[═"),(r p, "══"),(r $ r p, "═]")]
putVehicleOnMap ((Car, p, Vertical), c) m = foldr (uncurry Map.insert) m [(p,"┌┐"),(d p, "└┘")]
putVehicleOnMap ((Truck, p, Vertical), c) m = foldr (uncurry Map.insert) m [(p,"┌┐"),(d p, "││"),(d $ d p, "└┘")]

showBoard :: Board -> [String]
showBoard (Board s pvs) = map (\j -> concatMap (carSymbol j) [0..s-1]) [0..s-1] where
    carSymbol y x = Map.findWithDefault ".." (x,y) carMap
    carMap = foldr putVehicleOnMap Map.empty (zip pvs ['a'..])

showBoardWithHighlight :: Int -> Board -> [String]
showBoardWithHighlight i (Board s pvs) = map (\j -> concatMap (carSymbol j) [0..s-1]) [0..s-1] where
    carSymbol y x = Map.findWithDefault ".." (x,y) carMap'
    carMap = foldr putVehicleOnMap Map.empty (zip pvs ['a'..])
    carMap' = Set.foldr (uncurry Map.insert) carMap (Set.map (\p -> (p,"██")) $ allPositions (pvs !! i))

indexOfMovedCar :: Board -> Board -> Int
indexOfMovedCar (Board _ pvs) (Board _ pvs') = i where
    Just i = findIndex (uncurry (/=)) (zip pvs pvs') 

diffs :: [a] -> [(a,a)]
--diffs (a:as@(b:_)) = (a,b) : diffs as
--diffs _ = []
diffs as = zip as (tail as)

concatShownBoards :: [[String]] -> [String]
concatShownBoards = map (intercalate "  ") . transpose

showSolvedBoard :: [Board] -> [String]
showSolvedBoard bs = intercalate [[]] $ map showMove (diffs $ collapseMoves bs)

collapseMoves :: [Board] -> [Board]
collapseMoves (b:bs@(bb:bbs@(bbb:_))) = if (indexOfMovedCar b bb == indexOfMovedCar bb bbb) then 
    collapseMoves (b:bbs) else b : collapseMoves bs
collapseMoves bs = bs

showMove :: (Board, Board) -> [String]
showMove (b1, b2) = concatShownBoards (map (showBoardWithHighlight i) [b1,b2]) where
    i = indexOfMovedCar b1 b2

board0 = Board 6 [(Car, (4,0), Horizontal),(Truck, (2,0), Vertical),(Car, (0, 2), Horizontal), (Truck, (0,3), Horizontal),(Truck, (5,3), Vertical)]
board1 = Board 6 [(Truck, (0,0), Horizontal), (Truck, (3,0), Vertical), (Car, (5,0), Vertical), (Car, (0,1), Horizontal), (Car, (0,2), Vertical), (Car, (1,2), Horizontal), (Car, (5,2), Vertical), (Car, (2,3), Vertical), (Car, (3,3), Horizontal), (Car, (0,4), Horizontal), (Car, (4,4), Vertical), (Car, (0,5), Horizontal), (Car, (2,5), Horizontal)]
board2 = Board 6 [(Car, (2,0), Vertical),(Truck, (3,0), Horizontal),(Car, (0,1), Horizontal),
    (Car, (4,1), Vertical),(Car, (0,2), Vertical),(Car, (1,2), Horizontal),
    (Car, (3,2), Vertical),(Car, (4,3), Horizontal),(Car, (0,4), Horizontal),
    (Truck, (2,4), Horizontal),(Car, (5,4), Vertical),(Truck, (0,5), Horizontal)]
board4 = Board 6 [(Car, (0,0), Horizontal),(Car, (2,0), Horizontal),(Car, (4,0), Vertical),(Car,(5,0),Vertical),
    (Truck, (0,1), Vertical),(Car, (2,1), Horizontal),(Car,(1,2),Horizontal),
    (Car, (3,2), Vertical),(Car, (2,3), Vertical),(Car, (4,3), Horizontal),
    (Car, (3,4), Vertical),(Car, (4,4), Horizontal),(Truck, (0,5), Horizontal)]


main = do
    print "RushHour"
    putStrLn $ unlines $ showBoard board2
    putStrLn $ unlines $ showSolvedBoard (solveBoard board2)
    print "yeah"