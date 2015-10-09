{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HashMap where

import Debug.Trace
import Data.Word
import qualified Map as M

data Tree a = Leaf {value ::a} | Branch {false :: Tree a, true :: Tree a}
instance (Show a) => Show (Tree a) where
    show (Leaf a) = show a
    show (Branch f t) = "{" ++ show f ++ ", " ++ show t ++ "}"

getLeaf :: (Tree a) -> [Bool] -> a
getLeaf (Leaf a) _ = a
getLeaf (Branch _ _) [] = error "no leaf"
getLeaf (Branch f _) (False:bs) = getLeaf f bs
getLeaf (Branch _ t) (True:bs)  = getLeaf t bs

updateLeaf :: (Tree a) -> [Bool] -> (a -> a) -> (Tree a)
updateLeaf (Leaf a) _ f = (Leaf (f a))
updateLeaf t@(Branch _ _) [] _ = t
updateLeaf (Branch l r) (False:bs) f = Branch (updateLeaf l bs f) r
updateLeaf (Branch l r) (True:bs)  f = Branch l (updateLeaf r bs f)


toBinary :: (Bounded a, Enum a) => a -> [Bool]

toBinary a = reverse $ toBinaryLength range value where
    bounds :: Bounded a => a -> (a,a)
    bounds a = (minBound, maxBound)
    (minB, maxB) = bounds a
    range = (toInteger (fromEnum maxB) - toInteger (fromEnum minB) + 1)
    value = (toInteger (fromEnum a)) - (toInteger (fromEnum minB))
    toBinaryLength :: Integer -> Integer -> [Bool]
    toBinaryLength range value
        | (range<=1) = []
        | otherwise = (value `mod` 2 == 1) : toBinaryLength ((range+1) `div` 2) (value `div` 2)


fromBinary :: [Bool] -> Integer
fromBinary [] = 0
fromBinary (True:bs) = 1 + 2*fromBinary bs
fromBinary (False:bs) = 2*fromBinary bs

data Thousand = Thousand Int deriving Show

instance Enum Thousand where
  toEnum a = (Thousand (a `mod` 1000))
  fromEnum (Thousand a) = a

instance Bounded Thousand where
    minBound = Thousand 0
    maxBound = Thousand 999

makeTree :: Integer -> a -> Tree a
makeTree 0 a = Leaf a
makeTree n a = Branch t t where t = makeTree (n-1) a



getLeafNum :: (Enum i, Bounded i) => Tree a -> i -> a
getLeafNum t i = getLeaf t (toBinary i)

updateLeafNum :: (Enum i, Bounded i) => Tree a -> i -> (a -> a) -> (Tree a)
updateLeafNum t i f = updateLeaf t (toBinary i) f


t0 = makeTree 3 Nothing
t1 = updateLeaf t0 [False,True,True] (const $ Just 8)
t2 = updateLeaf t1 [True,False,False] (const $ Just 14)

class (Bounded i, Enum i) => Hash h a i | h -> a i where
 hash :: h -> a -> i
 inst :: h
 inst = undefined

data EnumHash e w
instance (Enum w, Bounded w, Enum e) => Hash (EnumHash [e] w) [e] w where
    hash _ es = (toEnum $ (h es) `mod` width)::w where
        width = (fromEnum(maxBound::w) - fromEnum(minBound::w))+1
        h [] = 0
        h (e:es) = fromEnum e + (h es)*33




data HashTree m h = HashTree {getTree:: Tree m} deriving Show
--instance Show (HashTree m h) where
 --   show ((HashTree m)::(HashTree m h)) = "HASHTREE " ++ showTree m where
        
        
    

instance (Hash h k i, M.Map m k v, Bounded i) => M.Map (HashTree m h) k v where
    get ((HashTree t)::(HashTree m h)) k = M.get (getLeafNum t (hash (inst::h) k)) k

    put ((HashTree t)::(HashTree m h)) k v = HashTree (updateLeafNum t (hash (inst::h) k) (\m -> M.put m k v))

    remove ((HashTree t)::(HashTree m h)) k = HashTree (updateLeafNum t (hash (inst::h) k) (\m -> M.remove m k))

    entries (HashTree t) = e t where
        e (Leaf v) = M.entries v
        e (Branch l r) = e l ++ e r

    empty = emptyTree::(HashTree m h) where
        hashfun = ((hash (inst::h)))::(k->i)
        emptyTree = HashTree (makeTree (toInteger $ length $ toBinary (minBound::i)) M.empty)
        
type HashMap h k v = HashTree (M.LinearMap k v) h

type StdEnumHashMap w k v  = HashMap (EnumHash k w) k v

myMap = M.empty::(StdEnumHashMap Thousand String String)


m0 = myMap
m1 = M.put m0 "cow" "moo"
m2 = M.put m1 "chicken" "cluck"
m3 = M.put m2 "pig" "oink"
m4 = M.putAll m3 [M.Entry (show i) (show (i*i)) | i<-[1,3..100]]
m5 = M.removeAll m4 [show i | i<-[7..40]]
m6 = M.put m5 "chicken" "bok bok"

main = do
    print "start"  
    print $ map (\x -> (x, hash (inst::EnumHash String Char) x)) (M.keys m6)
    