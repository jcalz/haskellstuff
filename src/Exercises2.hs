module Exercises2 where

import Exercises
import Debug.Trace

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
nand' True True = False
nand' _ _ = True

nor' :: Bool -> Bool -> Bool
nor' False False = True
nor' _ _ = False

not' :: Bool -> Bool
not' True = False
not' False = True

xor' :: Bool -> Bool -> Bool
xor' False x = x
xor' True x = not' x

impl' :: Bool -> Bool -> Bool
impl' True False = True
impl' _ _ = False

equ' :: Bool -> Bool -> Bool
equ' True x = x
equ' False x = not' x

table' :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table' f = [(a,b, f a b) | a <- [True, False], b <- [True, False]]

showTable' :: [(Bool, Bool, Bool)] -> String
showTable' = ("A  B  f(A,B)\n" ++) . (foldr f "") where
    f (a,b,fab) = ((s a ++ "  " ++ s b ++ "  " ++ s fab ++ "\n") ++) where
        s True = "T"
        s False = "F"

allPossibilities :: (Enum a, Bounded a) => Int -> [[a]]
allPossibilities 0 = [[]]
allPossibilities n = [a:as | a<-[minBound..maxBound], as<-(allPossibilities (n-1))]


tableN :: Int -> ([Bool] -> Bool) -> [([Bool],Bool)]
tableN n f = map (\as -> (as, f as)) (reverse $ allPossibilities n :: [[Bool]])

showTableN :: [([Bool],Bool)] -> String
showTableN = foldr f "" where
    f (as, fas) = ((foldr ((++) . (++ "  ") . s) (s fas ++ "\n") as) ++)
    s True = "T"
    s False = "F"

gray :: Int -> [String] -- n bit gray code
gray 0 = [""]
gray n = map ('0':) (gray (n-1)) ++ map ('1':) (reverse $ gray (n-1))

data BinaryTree a = Leaf a | Branch (BinaryTree a) (BinaryTree a) deriving Show
{--
iterateUntil :: (a -> Either a b) -> a -> b
iterateUntil f a = case (f a) of
    Left a' -> iterateUntil f a'
    Right b -> b
--}
iterateUntil :: (a -> Bool) -> (a -> a) -> a -> a
iterateUntil pred f a = if (pred a) then a else iterateUntil pred f (f a)

{--iterateUntilNothing :: (a -> Maybe a) -> a -> a
iterateUntilNothing f a = case (f a) of
    Just a' -> iterateUntilNothing f a'
    Nothing -> a--}
{--
huffman elems = iterateUntilNothing $ huffmanIterator (sortOn snd elems) [] where
    huffmanIterate :: ([(a,Double)], [(BinaryTree a, Double)]) -> Maybe ([(a,Double)], [(BinaryTree a, Double)])
    huffmanIterate ([],trees) = Nothing
    huffmanIterate (((e,fe):efs),[]) = Just (efs, [(Leaf e,fe)])
    huffmanIterate (((e,fe):efs),((t,ft):tfs)) = 
--}

removeMinBy :: (Ord o) => (a -> o) -> [a] -> Maybe (a,[a])
removeMinBy _ [] = Nothing
removeMinBy f (a:as) = case (removeMinBy f as) of
    Nothing -> Just (a,as)
    Just (b,bs) -> if (f a <= f b) then (Just (a,as)) else (Just (b, a:bs))
    
indexMap :: ((a,Int)->b) -> [a] -> [b]
indexMap f as = map f (zip as [0..])


huffman :: (Num b, Ord b, Show b) => [(a,b)] -> [(a, String)]
huffman elems =   map (\(i,s)->(fst (elems !! i),s)) $ sortOn (fst) (huffmanTreeProcess (huffmanTree (indexMap (\((a,b),i) -> (i,b)) elems))) where
    huffmanTreeProcess (Leaf a) = [(a,"")] 
    huffmanTreeProcess (Branch l r) = doBranch '0' l ++ doBranch '1' r where
        doBranch c t = map (\(a,s)->(a,c:s)) (huffmanTreeProcess t)

huffmanTree elems = fst $ head $ iterateUntil (\a -> length a <= 1) huffmanIterator ((map (\(a,f)->(Leaf a,f)) elems)) where
    huffmanIterator :: (Num b, Ord b, Show a, Show b) => [(BinaryTree a,b)] -> [(BinaryTree a,b)]
    huffmanIterator elems = (es ++ [(Branch a b , af+bf)])  where
        Just ((a, af),bs) = removeMinBy (snd) elems
        Just ((b, bf),es) = removeMinBy (snd) bs




 
data BinaryTree' a = Empty' | Branch' (BinaryTree' a) a (BinaryTree' a) 

allBalancedTrees :: Int -> [BinaryTree' Char]
allBalancedTrees 0 = [Empty']
allBalancedTrees k = [Branch' l 'x' r | (kl',kr') <- subTreeSizes k, l <- allBalancedTrees kl', r <- allBalancedTrees kr'] where
    subTreeSizes :: Int -> [(Int,Int)]
    subTreeSizes k = map (\i -> (i,k-1-i)) [((k-1)`div`2)..(k `div` 2)]


instance (Show a) => Show (BinaryTree' a) where
    show Empty' = "{}"
    show (Branch' l a r) = "{" ++ (show l) ++ (show a) ++ (show r) ++ "}"

{--prettyPrintBinaryTree :: (Show a) => BinaryTree' a -> String
prettyPrintBinaryTree Empty' = "∅"
prettyPrintBinaryTree (Branch' l a r) = "┌" ++ (replicate ((width pl)-1) '─') ++ pa ++ (replicate ((width pr)-1) '─') ++ "┐\n" 
    ++ merge pl pr (length pa) where        
        pl = prettyPrintBinaryTree l
        pr = prettyPrintBinaryTree r
        pa = show a
        merge x y s = undefined
--}

width :: String -> Int
width = foldr (max.length) 0 . lines

pad :: String -> String
pad x = unlines (map (\l -> l ++ replicate (w - length l) ' ' ) (lines x)) where 
    w = width x

horizJoin :: Int -> String -> String -> String
horizJoin s x y = unlines $ take (max (length lx) (length ly)) $ zipWith hj (lx ++ cycle [""]) (ly ++ cycle [""]) where
    lx = lines x
    ly = lines y
    hj :: String -> String -> String
    hj x' y' = x' ++ replicate (s + w - length x') ' ' ++ y'
    w = width x

prettyPrintBinaryTree :: (Show a) => BinaryTree' a -> String
prettyPrintBinaryTree Empty' = "Ø\n"
prettyPrintBinaryTree (Branch' l a r) = "┌" ++ (replicate ((width pl - 1)) '─') ++ pa ++ (replicate ((width pr - 1)) '─') ++ "┐\n" 
    ++ horizJoin (width pa) pl pr where
        pl = prettyPrintBinaryTree l
        pr = prettyPrintBinaryTree r
        pa = show a
        

isSymmetric :: BinaryTree' a -> Bool
isSymmetric Empty' = True
isSymmetric (Branch' l _ r) = isMirror l r where
    isMirror :: BinaryTree' a -> BinaryTree' b -> Bool
    isMirror Empty' Empty' = True
    isMirror _ Empty' = False
    isMirror Empty' _ = False
    isMirror (Branch' l1 _ r1) (Branch' l2 _ r2) = isMirror l1 r2 && isMirror r1 l2

addToSearchTree :: (Ord a) => a -> BinaryTree' a -> BinaryTree' a
addToSearchTree a Empty' = Branch' Empty' a Empty'
addToSearchTree a (Branch' l b r) = case (compare a b) of
    LT -> Branch' (addToSearchTree a l) b r
    EQ -> Branch' l a r -- Just replace 
    GT -> Branch' l b (addToSearchTree a r) 

constructBinarySearchTree :: (Ord a) => [a] -> BinaryTree' a
constructBinarySearchTree = foldl (flip addToSearchTree) Empty'


allSymmetricBalancedTrees :: Int -> [BinaryTree' Char]
allSymmetricBalancedTrees = (filter isSymmetric) . allBalancedTrees



allHeightBalancedTrees :: Int -> [BinaryTree' Char]
allHeightBalancedTrees 0 = [Empty']
allHeightBalancedTrees 1 = [Branch' Empty' 'x' Empty']
allHeightBalancedTrees k = [Branch' l 'x' r | (kl',kr') <- subTreeHeights k, l <- allHeightBalancedTrees kl', r <- allHeightBalancedTrees kr'] where
    subTreeHeights :: Int -> [(Int,Int)]
    subTreeHeights k = [(k-1,k-1), (k-1,k-2), (k-2,k-1)]

countNodes :: BinaryTree' a -> Int
countNodes Empty' = 0
countNodes (Branch' l a r) = 1 + countNodes l + countNodes r


--l = (map (\x -> if (x == ' ') then '\n' else x))

allHeightBalancedTreesWithNodeNumber :: Int -> [BinaryTree' Char]
allHeightBalancedTreesWithNodeNumber n = filter ((==n) . countNodes) $ concat $ map allHeightBalancedTrees [floor l .. ceiling l + 1] where
    l = logBase 2 (fromIntegral (n+1))

main = do
    print "start"
--    putStrLn $ showTable' $ table' (\a b -> and' a b)
--    putStrLn $ showTableN $ tableN 3 (\[a,b,c] -> (a `and'` b) `or'` c)
--    print $ huffman [('a',1),('b',1),('c',2),('d',4),('e',8),('f',16)]
--    print $ isSymmetric (Branch' (Branch' Empty' 'c' Empty') 'a' (Branch' Empty' 'b' Empty'))
    --print $ isSymmetric $ constructBinarySearchTree [3,2,5,7,1]
    --print $ allSymmetricBalancedTrees 9
    --print $ constructBinarySearchTree [3,2,5,7,1] 
    --putStrLn $ prettyPrintBinaryTree $ constructBinarySearchTree [3,2,5,7,1] 
    --putStrLn $ horizJoin 1 (l "What in the world") (l "are you talking about you crazy person?!") 
    --putStrLn $ show . length $ map prettyPrintBinaryTree (allHeightBalancedTreesWithNodeNumber 15)
    print "End"