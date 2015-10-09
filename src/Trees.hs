module Trees where

import Debug.Trace
import qualified Data.Foldable as F
import Data.Monoid

data BinaryTree a = Empty | Branch (BinaryTree a) a (BinaryTree a) 



preorder :: BinaryTree a -> [a]
preorder Empty = []
preorder (Branch l a r) = (a : preorder l) ++ preorder r

inorder :: BinaryTree a -> [a]
inorder Empty = []
inorder (Branch l a r) = inorder l ++ (a : inorder r)

treeToString :: (Show a) => BinaryTree a -> String
treeToString Empty = ""
treeToString (Branch l a r) = (show a) ++ 
    case (l,r) of 
        (Empty,Empty) -> ""
        _ -> "(" ++ treeToString l ++ "," ++ treeToString r ++ ")"

instance (Show a) => Show (BinaryTree a) where
  show = treeToString
                                                         

leaf :: a -> BinaryTree a
leaf = (flip (Branch Empty)) Empty

treeTest = Branch (Branch (leaf 'D') 'B' (leaf 'E')) 'A' (Branch Empty 'C' (leaf 'F'))

dotstring :: BinaryTree Char -> String
dotstring Empty = "."
dotstring (Branch l a r) = [a] ++ (dotstring l) ++ (dotstring r)

undotstring :: String -> BinaryTree Char
undotstring = fst . ui where
    ui :: String -> (BinaryTree Char, String)
    ui "" = (Empty, "")
    ui ('.':s) = (Empty, s)
    ui (c:s) = (Branch l c r, s'') where
        (l, s') = ui s
        (r, s'') = ui s'

preorderToTree :: [a] -> BinaryTree a
preorderToTree [] = Empty
preorderToTree (a:as) = Branch Empty a (preorderToTree as)

ordersToTree :: (Eq a) => [a] -> [a] -> BinaryTree a
ordersToTree [] [] = Empty
ordersToTree (p:ps) is = Branch l p r where
    (isL,_:isR) = takeDropWhile (/= p) is
    (psL,psR) = takeDropWhile (`elem` isL) ps
    l = ordersToTree psL isL
    r = ordersToTree psR isR
    
takeDropWhile :: (a -> Bool) -> [a] -> ([a],[a])
takeDropWhile pred [] = ([],[])
takeDropWhile pred (a:as) = if (pred a) then
    let (t,d) = takeDropWhile pred as
        in (a:t, d)
    else
        ([],(a:as))
        
goThroughMotions tree = let
    p = preorder tree
    i = inorder tree
    in do
    print tree
    print p
    print i
    print $ ordersToTree p i

g2m2 tree = let
    d = dotstring tree
    in do
        print tree
        print d
        print $ undotstring d `asTypeOf` tree

g3m3 tree = let
    p = preorder tree
    t = preorderToTree p
    pp = preorder t
    in do
        print tree
        print p
        print t
        print pp


data Tree a = Node a [Tree a] deriving (Eq, Show)

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

instance (F.Foldable Tree) where
    foldMap f (Node a ts) = mappend (f a) (foldr (mappend.(F.foldMap f)) mempty ts)

countNodes :: Tree a -> Int
countNodes = F.foldr (const (+1)) 0

caretString :: Tree Char -> String
caretString = fst . caretStringAndFinalDepth where
    caretStringAndFinalDepth :: Tree Char -> (String, Int)
    caretStringAndFinalDepth (Node a ts) = (caretString, depth) where
            subs = map caretStringAndFinalDepth ts
            (subString,subDepth) = foldl (\(acc,_) (s,d) -> (acc ++ s ++ (replicate d '^'), d)) ("",0) subs
            caretString = a : subString ++ "^"
            depth = subDepth


uncaretString :: String -> Tree Char
uncaretString s = t where
    (Just t, _) = uu s
    uu :: String -> (Maybe (Tree Char), String)
    uu ('^':s) = (Nothing, s)
    uu (c:s) = (Just (Node c ts), s') where
        (ts,s') = uuCollect s
    uuCollect :: String -> ([Tree Char], String)
    uuCollect s = case (uu s) of
        (Nothing, r) -> ([], r)
        (Just t, r) -> (t : ts, r') where
            (ts, r') = uuCollect r
        
g4m4 tree = let
    cs = caretString tree
    ts = uncaretString cs 
    in do
        print tree
        print cs
        print ts

--foldrChunks :: (a -> f)
--foldrChunks = undefined


ipl :: Tree a -> Int
ipl (Node _ ts) = (foldr (\t -> (countNodes t + ipl t +)) 0) ts 

bottomUp :: Tree a -> [a]
bottomUp (Node a ns) = concat (map bottomUp ns) ++ [a]

lispy :: Tree Char -> String
lispy (Node a []) = [a]
lispy (Node a ts) = '(' : a : concat (map ((' ':) . lispy) ts) ++ ")"

unlispy :: String -> Tree Char
unlispy s = t where
    (Just t, _) = (uu (filter (/= ' ') s))
    uu :: String -> (Maybe (Tree Char), String)
    uu "" = (Nothing, "")
    uu (')':s) = (Nothing, s)
    uu ('(':a:s) = (Just (Node a ts),s') where
        (ts, s') = uuCollect s
    uu (a:s) = (Just (Node a []),s)       
    uuCollect :: String -> ([Tree Char], String)
    uuCollect s = case (uu s) of
        (Nothing, r) -> ([], r)
        (Just t, r) -> (t : ts, r') where
            (ts, r') = uuCollect r
    

main = do
    print "Start"
    --print tree5
    --print $ caretString (Node 'a' [Node 'b' [], Node 'c' []])
    --g3m3 treeTest
    --g4m4 tree5
    print $ tree5
    print $ lispy tree5
    print $ unlispy $ lispy tree5
    print "End"