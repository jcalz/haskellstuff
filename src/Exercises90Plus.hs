module Exercises90Plus where

-- Eight Queens

extract :: Int -> [a] -> (a, [a])
extract i as = (as !! i, take i as ++ drop (i+1) as)

perms :: [a] -> [[a]]
perms [] = [[]]
perms as = concatMap (\i -> let (a, as') = extract i as in map (a:) (perms as')) [0..length as -1]


insertAt :: a -> Int -> [a] -> [a]
insertAt a 0 bs = a:bs
insertAt a n (b:bs) = b:(insertAt a (n-1) bs) 
insertAt a n [] = error "cannot insert past the end"

insertEverywhere :: a -> [a] -> [[a]]
insertEverywhere a as = map (flip (insertAt a) as) [0..(length as)]


{--choose :: Int -> [a] -> [[a]]
choose 0 _ = []
choose 1 [] = []
choose 1 ()--}

choose2 :: [a] -> [[a]]
choose2 as = [ [(as !! i), (as !! j)] | i <- [0 .. length as - 1] , j <- [i+1 .. length as - 1] ]

canAttack :: (Int,Int) -> (Int,Int) -> Bool
canAttack (x,y) (x',y') = (x-x')^2 == (y-y')^2

queensAttack :: [Int] -> (Int,Int) -> Bool
queensAttack as (i,j) = canAttack (i, as!!i) (j, as!!j)

queensAnyAttack :: [Int] -> Bool
queensAnyAttack as = any (queensAttack as) [(i,j) | i <- [0..length as - 1], j <- [i+1 .. length as -1]]

queens :: Int -> [[Int]]
queens n = filter (not . queensAnyAttack) (perms [1..n])


-- KNIGHT'S TOUR --

type BoardWidth = Int
type BoardPosition = (Int, Int)
type BoardPath = [(Int,Int)] -- in reverse order

allValidBoardPositions :: BoardWidth -> [BoardPosition]
allValidBoardPositions i = [(x,y) | x<-[0..(i-1)], y<-[0..(i-1)]]

validKnightMoves :: BoardWidth -> BoardPosition -> [BoardPosition]
validKnightMoves i (x,y) = filter (\(x,y) -> x>=0 && y>=0 && x<i && y<i) $ map (\(x',y') -> (x+x',y+y')) [(2,1),(1,2),(-1,2),(-2,1),(-2,-1),(-1,-2),(1,-2),(2,-1)]

validNewKnightMoves :: BoardWidth -> BoardPath -> [BoardPosition]
validNewKnightMoves i [] = allValidBoardPositions i
validNewKnightMoves i ps@(p:_) = filter (not . (`elem` ps)) $ validKnightMoves i p

stepPaths :: BoardWidth -> [BoardPath] -> [BoardPath]
stepPaths i ps = [pos:path | path <- ps, pos <- validNewKnightMoves i path]

maxExtendPaths :: BoardWidth -> [BoardPath] -> [BoardPath]
maxExtendPaths i ps = case (stepPaths i ps) of
    [] -> ps
    ps' -> maxExtendPaths i ps'

maxExtendOnePath :: BoardWidth -> BoardPath -> BoardPath
maxExtendOnePath i p = case (stepPaths i [p]) of
    [] -> p
    p':_ -> maxExtendOnePath i p'



main = do
    print "start Ex90+"
    print $ head $ (maxExtendPaths 4 [[]])
    print "end Ex90+"