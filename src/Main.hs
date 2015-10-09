{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}

module Main where

import Data.Function


splitfilter :: (a->Bool) -> [a] -> ([a],[a])
splitfilter _ [] = ([],[])
splitfilter f (x:xs) 
 | f x = (x:y, n)
 | otherwise = (y, x:n)
    where (y,n) = splitfilter f xs

partition :: [a->Bool] -> [a] -> [[a]]
partition [] _ = []
partition (f:fs) x = y : partition fs n where (y,n)=splitfilter f x
                                            




x `divides` y = y `mod` x == 0

things = [46, 89, 7, 90, 65, 41, 24, 24, 7, 69, 23, 42, 94, 77, 6, 25, 65, 23, 71, 69]

--main = print $ partition [(2 `divides`),(5 `divides`),(\x->otherwise)] [1..20]


quicksortby :: (a -> a-> Bool) -> [a] -> [a]
quicksortby _ [] = []
quicksortby less (x:xs) = quicksortby less l ++ (x:quicksortby less r) where (l,r)= splitfilter (`less` x) xs

quicksortwith :: Ord b => (a -> b) -> [a] -> [a]
quicksortwith t = quicksortby ( (<) `on` t )

quicksort :: Ord a => [a] -> [a]
quicksort = quicksortwith id


data ComparatorContainer a = ComparatorContainer (a -> a -> Bool)




    

xx = ComparatorContainer (\x y -> x `div` 10 < y `div` 10) 



{-data Nat = Z | S Nat

type family (m :: Nat) :+ (n :: Nat) :: Nat
type instance m :+ Z = m
type instance m :+ (S a) = S (m :+ a)

foo :: Nat
foo = Z :+ Z
-}

--newtype Pair a b=Pair (forall c.(a->b->c)->c)

--nadd :: (Nat a, Nat b) => a -> b -> c

data Box b = Box b

--nadd :: (Nat a, Nat b) => a -> b -> (forall c. (Nat c)=>c)
--nadd a b = Z

--nadd x Z = x
--nadd x (S a) = S (nadd x a)

--nadd x (S a) = S (nadd x a)


data Pair a b = Pair a b
pfirst (Pair a _) = a
pSecond (Pair _ b) = b

--nadd :: (Nat a, Nat b) => a -> b -> ((Nat c) => c)

data Z = Z
data S a = S a

class Nat a 

instance Nat Z

instance (Nat a) => Nat (S a)

--type family 

--data (Nat n) => Vec (S n) a = Vec n (Vec n a) | Nil

--data Vec m n a = Vec a (Vec m a) | Nil

--data (Nat n, Nat m) => Vec m n a = Vec m a | Nill



data (Nat n) => Vector n a = (n ~ Z) => X | forall m. (n ~ S m) => V a (Vector m a)

--makeVec 0 = X

a .: v =  V a v
infixr 5 .:

(.!!) :: (Integral i) => (Vector n a) -> i -> a
(V a _) .!! 0 = a
(V a v) .!! n = v .!! (n-1)
X .!! _ = error "Can't read past the end of the list"

vLength :: (Vector n a ~ v, Integral i) => v -> i
vLength (V a v) = 1 + vLength v
vLength X = 0

--vFromList :: [a] -> Vector n a

--vFromList :: n -> [a] -> Vector n a

--vFromList Z _ = X

--vFromList (S n) (x:xs) = V x (vFromList n xs)


foo = 8 .: 6 .: 7 .: 5 .: 3 .: 0 .: 9 .: X

goo = map (foo .!!) [0..(vLength foo)-1]

-- | The main entry point.
main :: IO ()
main = do
    print $ goo
    print "done"
    