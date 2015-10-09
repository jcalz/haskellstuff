{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances, IncoherentInstances #-}

{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, GADTs #-}

module SizedVector where

data NatK = ZT | ST NatK


type family (n :: NatK) :+ (m :: NatK) :: NatK
type instance ZT :+ m = m
type instance (ST n) :+ m = ST(n :+ m)


type family (n :: NatK) :* (m :: NatK) :: NatK
type instance ZT     :* m = ZT
type instance (ST n) :* m = (n :* m) :+ m

type family (n :: NatK) :× (m :: NatK) :: NatK
type instance ZT     :× m = ZT
type instance (ST n) :× m = m :+ (n :× m) 


-- this "subtraction" returns zero when the actual result would be negative.
type family (n :: NatK) :- (m :: NatK) :: NatK
type instance n :- ZT = n
type instance (ST n) :- (ST m) = n :- m
type instance ZT :- (ST m) = ZT

type family Min (n :: NatK) (m :: NatK) :: NatK
type instance Min n ZT = ZT
type instance Min ZT n = ZT
type instance Min (ST m) (ST n) = ST (Min m n)


data Vector a n where
  Nil  :: Vector a ZT
  (:=) :: a -> Vector a n -> Vector a (ST n)
infixr 5 :=

data Nat n where
    Z :: Nat ZT
    S :: Nat n -> Nat (ST n)
deriving instance Eq (Nat n)
deriving instance Show (Nat n)


n0 = Z
n1 = S n0
n2 = S n1
n3 = S n2
n4 = S n3
n5 = S n4
n6 = S n5
n7 = S n6
n8 = S n7
n9 = S n8
n10 = S n9

type XT = ST(ST(ST(ST(ST(ST(ST(ST(ST(ST ZT)))))))))

(<^>) :: (u ~ (u :+ ZT)) => Nat t -> Nat u -> Nat ((t :× XT) :+ u) 
Z <^> u = u
(S t) <^> u = S(S(S(S(S(S(S(S(S(S( t <^> u ))))))))))


toInt :: Nat n -> Int
toInt Z = 0
toInt (S n) = 1 + toInt n 

deriving instance Eq a => Eq (Vector a n)
deriving instance Show a => Show (Vector a n)


head :: Vector a (ST n) -> a
head (x := xs) = x
vhead = SizedVector.head

tail :: Vector a (ST n) -> Vector a n
tail (x := xs) = xs
vtail = SizedVector.tail

append :: Vector a m -> Vector a n -> Vector a (m :+ n)
append Nil v = v
append (x := xs) v = x := (append xs v)
(-++) = SizedVector.append

at ::  ( (ST i :- n) ~  ZT) => Vector a n -> Nat i -> a 
at (x := _) Z = x
at (_ := xs) (S n) = at xs n
(-!!) ::  ( (ST i :- n) ~ ZT) => Vector a n -> Nat i -> a 
(-!!) = SizedVector.at

length :: Vector a n -> Nat n
length Nil = Z
length (x := xs) = S(SizedVector.length xs)
vlength = SizedVector.length

toList :: Vector a n -> [a]
toList Nil = []
toList (x := xs) = x: toList xs

fromList :: Nat n -> [a] -> Vector a n
fromList Z _ = Nil
fromList (S n) (x:xs) = x := fromList n xs

map :: (a -> b) -> Vector a n -> Vector b n
map f Nil = Nil
map f (x:=xs) = (f x) := SizedVector.map f xs
vmap = SizedVector.map

uncons :: Vector a n -> Maybe (a, Vector a (n :- ST ZT))
uncons Nil = Nothing
uncons (x:=xs@(_:=_)) = Just (x,xs)
vuncons = SizedVector.uncons

init :: Vector a (ST n) -> Vector a n
init (x:=Nil) = Nil
init (x:=xs@(_:=_)) = x:= (SizedVector.init xs)
vinit = SizedVector.init

last :: Vector a (ST n) -> a
last (x:=Nil) = x
last (x:=xs@(_:=_)) = SizedVector.last xs
vlast = SizedVector.last

zipWithSame :: (a->b->c) -> Vector a n -> Vector b n -> Vector c n
zipWithSame _ Nil Nil = Nil
zipWithSame f (a:=as) (b:=bs) = (f a b) := (zipWithSame f as bs)

zipWith :: (a->b->c) -> Vector a nn -> Vector b mm -> Vector c (Min mm nn)
zipWith _ Nil _ = Nil
zipWith _ _ Nil = Nil
zipWith f (a:=as) (b:=bs) = (f a b) := (SizedVector.zipWith f as bs)
vzipWith = SizedVector.zipWith

zipWithSame' :: (Min n n ~ n) => (a->b->c) -> Vector a n -> Vector b n -> Vector c n
zipWithSame' = SizedVector.zipWith

reverse :: ((n :+ ST ZT) ~ ST n) => Vector a n -> Vector a n
reverse Nil = Nil
reverse (x:=xs) = SizedVector.reverse xs -++ (x:=Nil)
vreverse :: ((n :+ ST ZT) ~ ST n) => Vector a n -> Vector a n
vreverse = SizedVector.reverse

zipWithCartesian ::  (a->b->c) -> Vector a nn -> Vector b mm -> Vector c (nn :× mm)
zipWithCartesian f (a:=as) bs = (vmap (f a) bs) -++ (zipWithCartesian f as bs)   
zipWithCartesian f Nil _ = Nil

addVec :: (Num a) => Vector a n -> Vector a n -> Vector a n 
addVec v1 v2 = zipWithSame (+) v1 v2

foldr :: (a->b->b) -> b -> Vector a n -> b
foldr f z Nil   = z
foldr f z (x:=xs) = f x (SizedVector.foldr f z xs)
vfoldr = SizedVector.foldr

dotVec :: (Num a) => Vector a n -> Vector a n -> a
dotVec v1 v2 = (vfoldr (+) 0  (zipWithSame (\x y -> x*y) v1 v2))

normVec :: (Floating a) => Vector a n -> a
normVec v = sqrt (dotVec v v)


foo = fromList n7 [8,6,7,5,3,0,9]
goo = 3 := 5 := Nil
v1 = fromList n5 [1,2,3,4,5]
v2 = fromList n5 [6,7,8,9,10]
v3 = vreverse v1

v4 = zipWithCartesian (,) goo v2

doAThing :: (ST n ~ (n :+ ST ZT)) => Vector a n -> Vector (a,a) n
doAThing x = (zipWithSame (,) x (vreverse x))

hoo = vzipWith (-) v1 v2

ioo = goo -++ foo

--num = n1 <^> n3 <^> n9 -- 139

main = do  
  print $ toList v3
  print $ normVec v3
  print $ foo `at` n0
  print $ toList v4
  print $ toList $ vreverse ioo
  print $ toInt $ n1 <^> n7 <^> n6
  print $ toList $ (doAThing foo)
  print "done"
    
