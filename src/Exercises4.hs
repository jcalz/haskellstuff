module Exercises4 where

import qualified Exercises as E
import qualified Exercises2 as E

import qualified Control.Monad as Monad
import qualified Data.List as List
import Debug.Trace

data BinaryTree a = Empty | Branch (BinaryTree a) a (BinaryTree a)


width :: String -> Int
width = foldr (max.length) 0 . lines

horizJoin :: Int -> String -> String -> String
horizJoin s x y = unlines $ take (max (length lx) (length ly)) $ zipWith hj (lx ++ cycle [""]) (ly ++ cycle [""]) where
    lx = lines x
    ly = lines y
    hj :: String -> String -> String
    hj x' y' = x' ++ replicate (s + w - length x') ' ' ++ y'
    w = width x

prettyPrintBinaryTree :: (Show a) => BinaryTree a -> String
prettyPrintBinaryTree Empty = "Ø\n"
prettyPrintBinaryTree (Branch l a r) = "┌" ++ (replicate ((width pl - 1)) '─') ++ pa ++ (replicate ((width pr - 1)) '─') ++ "┐\n" 
    ++ horizJoin (width pa) pl pr where
        pl = prettyPrintBinaryTree l
        pr = prettyPrintBinaryTree r
        pa = show a
        
instance (Show a) => Show (BinaryTree a) where
    show = prettyPrintBinaryTree


countLeaves :: BinaryTree a -> Int
countLeaves Empty = 0
countLeaves (Branch Empty _ Empty) = 1
countLeaves (Branch l _ r) = countLeaves l + countLeaves r

leaves :: BinaryTree a -> [a]
leaves Empty = []
leaves (Branch Empty a Empty) = [a]
leaves (Branch l _ r) = leaves l ++ leaves r

internals :: BinaryTree a -> [a]
internals Empty = []
internals (Branch Empty a Empty) = []
internals (Branch l a r) = internals l ++ (a : internals r)

atLevel :: BinaryTree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch _ a _) 1 = [a]
atLevel (Branch l _ r) n = atLevel l (n-1) ++ atLevel r (n-1)

levelThingy :: Int -> Int
levelThingy n = 2 ^ (clog2 ((n-1) `div` 2 + 1)) - 1 where
    clog2 = ceiling . (logBase 2) . fromIntegral

completeBinaryTree :: Int -> BinaryTree Char
completeBinaryTree 0 = Empty
completeBinaryTree n = Branch (completeBinaryTree ln) 'x' (completeBinaryTree rn) where
    completeDepth = floorLog2 (n + 1)
    completeNodes = 2 ^ completeDepth - 1
    extraNodes = n - completeNodes
    roomOnLeftSide = 2 ^ (completeDepth - 1)
    extraNodesOnLeftSide = min extraNodes roomOnLeftSide
    extraNodesOnRightSide = extraNodes - extraNodesOnLeftSide
    completeNodesOnEachSide = (completeNodes - 1) `div` 2
    ln = completeNodesOnEachSide + extraNodesOnLeftSide
    rn = completeNodesOnEachSide + extraNodesOnRightSide

hasSameStructure :: BinaryTree a -> BinaryTree b -> Bool
hasSameStructure Empty Empty = True
hasSameStructure (Branch al _ ar) (Branch bl _ br) = hasSameStructure al ar && hasSameStructure bl br
hasSameStructure _ _ = False

countNodes :: BinaryTree a -> Int
countNodes Empty = 0
countNodes (Branch l _ r) = 1 + countNodes l + countNodes r

isCompleteBinaryTree :: BinaryTree a -> Bool
isCompleteBinaryTree t = hasSameStructure t (completeBinaryTree (countNodes t))

--makeAllCompletelyBalancedTrees :: Int -> [BinaryTree Char]
--makeAllCompletelyBalancedTrees 0 = [Empty]
--makeAllCompletelyBalancedTrees n = [Branch l 'x' r | 
--    nl <- [0..n-1], nr <- [0..nl], nl + nr == n-1, 
--    l <- makeAllCompletelyBalancedTrees nl,
--    r <- makeAllCompletelyBalancedTrees nr]

floorLog2 :: Int -> Int
floorLog2 = length . takeWhile (> 1) . (iterate (`div` 2)) 

collectInOrder :: BinaryTree a -> [a]
collectInOrder Empty = []
collectInOrder (Branch l a r) = collectInOrder l ++ a : collectInOrder r

instance Functor BinaryTree where
    fmap f Empty = Empty
    fmap f (Branch l a r) = Branch (fmap f l) (f a) (fmap f r)

augmentWithPosition :: BinaryTree a -> BinaryTree (a, Int)
augmentWithPosition Empty = Empty
augmentWithPosition (Branch l a r) = Branch al (a, pa) ar where
    al = augmentWithPosition l
    pa = countNodes al
    ar = fmap (\(e, p) -> (e, pa+p+1)) (augmentWithPosition r)


augmentWithPositionAndDepth :: BinaryTree a -> BinaryTree (a, (Int, Int))
augmentWithPositionAndDepth Empty = Empty
augmentWithPositionAndDepth (Branch l a r) = Branch al (a, (pa, 1)) ar where
    al = fmap (\(e,(p,d)) -> (e, (p, d+1))) (augmentWithPositionAndDepth l)
    pa = countNodes al
    ar = fmap (\(e,(p,d)) -> (e, (pa+p+1, d+1))) (augmentWithPositionAndDepth r)

layout = augmentWithPositionAndDepth

totalDepth :: BinaryTree a -> Int
totalDepth Empty = 0
totalDepth (Branch l _ r) = 1 + max (totalDepth l) (totalDepth r)



layout' t = fmap (\(a,(p,d)) -> (a,((2*p+1)*2^(totalDepth t-d)-1,d))) (layoutInner t)

layoutInner Empty = Empty
layoutInner t@(Branch l a r) = Branch l' (a, (0,1)) r' where
    l' = fmap (\(a,(p,d)) -> (a,(p,d+1))) (layoutInner l)
    r' = fmap (\(a,(p,d)) -> (a,(2^(d-1)+p,d+1))) (layoutInner r)

isNonEmpty :: [a] -> Bool
isNonEmpty [] = False
isNonEmpty _ = True

nodesByDepth :: BinaryTree a -> [[a]]
nodesByDepth = takeWhile isNonEmpty . nodesByDepth' where
    nodesByDepth' Empty = cycle [[]]
    nodesByDepth' (Branch l a r) = [a] : (zipWith (++) (nodesByDepth' l) (nodesByDepth' r))

maxAll :: (Ord a) => a -> [a] -> a
maxAll def [] = def
maxAll _ (a:as) = foldr max a as

minAll :: (Ord a) => a -> [a] -> a
minAll def [] = def
minAll _ (a:as) = foldr min a as




type PositionedTree a = BinaryTree (a, (Int, Int))

translateTree :: (Int, Int) -> PositionedTree a -> PositionedTree a
translateTree _ Empty = Empty
translateTree dp@(dx,dy) (Branch l (a,(x,y)) r) = Branch (translateTree dp l) (a,(x+dx,y+dy)) (translateTree dp r)

maybefy :: (a -> a -> a) -> (Maybe a -> Maybe a -> Maybe a)
maybefy _ Nothing Nothing = Nothing
maybefy _ Nothing y = y
maybefy _ x Nothing = x
maybefy f (Just x) (Just y) = Just (f x y)

maybeMin :: (Ord a) => (Maybe a -> Maybe a -> Maybe a)
maybeMin = maybefy min

maybeMax :: (Ord a) => (Maybe a -> Maybe a -> Maybe a)
maybeMax = maybefy max

maybeMinAll :: (Ord a) => [Maybe a] -> Maybe a
maybeMinAll = foldr maybeMin Nothing

maybeMaxAll :: (Ord a) => [Maybe a] -> Maybe a
maybeMaxAll = foldr maybeMax Nothing

getXPos :: (a,(Int,Int)) -> Int
getXPos (_,(x,_)) = x

mostAtEachDepth :: (Int -> Int -> Int) -> PositionedTree a -> [Maybe Int]
mostAtEachDepth f t = map ((foldr (maybefy f) Nothing) . (map (Just . getXPos))) (nodesByDepth t)

leftMostPosition :: PositionedTree a -> Maybe Int
leftMostPosition Empty = Nothing
leftMostPosition (Branch l (a,(x,y)) r) = maybeMinAll [leftMostPosition l, Just x, leftMostPosition r]

horizontallyPositionTree :: PositionedTree a -> PositionedTree a
horizontallyPositionTree t = case (leftMostPosition t) of
    Nothing -> t
    Just x -> translateTree (1-x,0) t

horizontalOverlapsAtEachDepth ta tb = zipWith (Monad.liftM2 (-)) 
    ((fmap.fmap) (+1) $ mostAtEachDepth max ta)  (mostAtEachDepth min tb)  

horizontalOverlap :: PositionedTree a -> PositionedTree b -> Maybe Int
horizontalOverlap ta tb = maybeMaxAll (horizontalOverlapsAtEachDepth ta tb)
   
horizontallyPrepareRightTree :: PositionedTree a -> PositionedTree b -> (Int, PositionedTree b)
horizontallyPrepareRightTree Empty Empty = (1, Empty)
horizontallyPrepareRightTree l@(Branch _ (_,(n,_)) _) Empty = (n+1, Empty)
horizontallyPrepareRightTree Empty r@(Branch _ (_,(n,_)) _) = (n-1, r)
horizontallyPrepareRightTree l@(Branch _ (_,(nl,_)) _) r@(Branch _ (_,(nr,_)) _) = (parentPos, r') where
    overlap = case (horizontalOverlap l r) of
        Nothing -> error "this is impossible"
        Just o -> o
    nr' = nr + overlap
    diff = nr' - nl
    diffFixed = ((diff + 1) `div` 2) * 2
    nrFixed = nl + diffFixed
    parentPos = nl + (diffFixed `div` 2)
    r' = translateTree (nrFixed - nr, 0) r
    
layoutCompact :: BinaryTree a -> PositionedTree a
layoutCompact Empty = Empty
layoutCompact (Branch l a r) = horizontallyPositionTree $ Branch l' (a,(pos,1)) r' where
    lsub = layoutCompact l
    rsub = layoutCompact r
    (pos, rslid) = horizontallyPrepareRightTree lsub rsub
    l' = translateTree (0,1) lsub
    r' = translateTree (0,1) rslid

listToLeftTree :: [a] -> BinaryTree a
listToLeftTree [] = Empty
listToLeftTree (a:as) = Branch (listToLeftTree as) a Empty


tree4 = Branch (Branch Empty 'a' (Branch Empty 'b' Empty)) 'c' (Branch Empty 'd' Empty)

flipBranch = flip Branch

tree65 = flipBranch 'n'
                (flipBranch 'k'
                        (flipBranch 'c'
                                (flipBranch 'a' Empty Empty)
                                (flipBranch 'e'
                                        (flipBranch 'd' Empty Empty)
                                        (flipBranch 'g' Empty Empty)
                                )
                        )
                        (flipBranch 'm' Empty Empty)
                )
                (flipBranch 'u'
                        (flipBranch 'p'
                                Empty
                                (flipBranch 'q' Empty Empty)
                        )
                        Empty
                )

leaf a = Branch Empty a Empty

pt0 = Branch (leaf ('a',(1,2))) ('b',(2,1)) Empty

pt1 = leaf ('c',(1,1))

pt2 = Branch (leaf ('d',(1,2))) ('e',(2,1)) (leaf ('f',(3,2)))


escSpecialChars :: [Char] -> Char -> String -> String
escSpecialChars special esc a = foldr f "" a where
    f = (\c cs -> if (c `elem` (esc:special)) then esc:(c:cs) else c:cs)

parseA :: [Char] -> Char -> String -> [(Bool,Char)]
parseA specials escChar str = reverse $ snd $ List.foldl' f (False,[]) str where
    f (True, acc) c = (False, (False,c):acc)
    f (False, acc) c 
        | c == escChar = (True, acc)
        | c `elem` specials = (False, (True, c):acc)
        | otherwise = (False, (False, c):acc)

data Token = StringToken String | SpecialCharacter Char deriving Show
    
parseB :: [(Bool,Char)] -> [Token]
parseB = foldr f [] where
    f (True,c) acc = (SpecialCharacter c) : acc 
    f (False, c) ((StringToken s):acc) = (StringToken (c:s)):acc
    f (False, c) acc = (StringToken [c]):acc

parse :: [Char] -> Char -> String -> [Token]
parse s e = parseB . (parseA s e)

{--parseWithSpecialChars :: [Char] -> Char -> String -> [Either String Char]
parseWithSpecialChars special esc "" = []
parseWithSpecialChars special esc (a:as) =
    if (a == special) then--}

{--splitOnSpecialCharsWithEscape :: [Char] -> Char -> String -> [Either String Char]
splitOnSpecialCharsWithEscape specials escChar s = splitOnSpecialCharsWithEscapeInner s [] where
    splitOnSpecialCharsWithEscapeInner :: String -> [Either String Char] -> [Either String Char]
    splitOnSpecialCharsWithEscapeInner "" x = x
    splitOnSpecialCharsWithEscapeInner (c:cs) [] = splitOnSpecialCharsWithEscapeInner cs [Left [c]]
    splitOnSpecialCharsWithEscapeInner (c:cs) ((Right sc):es) =
        if (c == escChar) then
            splitOnSpecialCharsWithEscapeInner cs (case es of 
                ((Left st):ees) -> (Left (sc:st):ees)
                _ -> (Left [sc]:es))
        else
            case es of    
 --}



treeToString :: (Show a) => BinaryTree a -> String
treeToString Empty = ""
treeToString (Branch l a r) = (escSpecialChars "(,)" '\\' (show a)) ++ subs l r where 
    subs Empty Empty = ""
    subs l r = "(" ++ treeToString l ++ "," ++ treeToString r ++ ")"



testStr =  "I \\definitely\\ said \"Howdy!!\""

esc' = escSpecialChars "\"" '\\'
parse' = parse "\"" '\\'

tailRecurse :: (i -> Either i o) -> i -> o
tailRecurse f i = case (f $! i) of 
    Left i' -> tailRecurse f i'
    Right o -> o

reverse' :: [a] -> [a]
reverse' as = tailRecurse f (as,[]) where
    f (a:as,r) = Left (as,a:r)
    f ([],r) = Right r


stringToTree :: (Read a) => String -> BinaryTree a
stringToTree s = fst $ walkTokens (parse "(,)" '\\' s) where
    walkTokens :: (Read a) => [Token] -> (BinaryTree a, [Token])
    walkTokens [] = (Empty, [])
    walkTokens (SpecialCharacter '(':tokens) = (Branch l undefined r, tokens'') where
        (l, SpecialCharacter ',' : tokens') = walkTokens tokens
        (r, SpecialCharacter ')' : tokens'') = walkTokens tokens'
    walkTokens ts@(SpecialCharacter sc:tokens) = (Empty, ts)
    walkTokens (StringToken s:tokens) = (Branch l a r, tokens') where
        a = read s
        (sub, tokens') = walkTokens tokens
        (l,r) = case sub of 
            (Branch l' _ r') -> (l',r')
            (Empty) -> (Empty,Empty)

main = do
    print "Start"
    --print $ tree65
    --print pt0
    --print pt1
    --print $ layoutCompact (Branch  (listToLeftTree "hij") 'g' (listToLeftTree "abcdef"))
    print (tree65)
    putStrLn (treeToString tree65)
    print $ (stringToTree (treeToString tree65) :: BinaryTree Char)
    --print $ parse' $ "\"  " ++ esc' testStr ++ "  \"" 
    print "Ending"