{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Matrix where

import Debug.Trace

data NatK where
    ZK :: NatK
    SK :: NatK -> NatK

type family Plus (a::NatK) (b::NatK) :: NatK where
    Plus ZK b = b
    Plus (SK a) b = SK (Plus a b)

type family Minus (a::NatK) (b::NatK) :: NatK where
    Minus ZK b = ZK
    Minus a ZK = a
    Minus (SK a) (SK b) = Minus a b

type Leq (a::NatK) (b::NatK) = (Minus a b ~ ZK)

type Less (a::NatK) (b::NatK) = (Leq (SK a) b)

data Nat (n :: NatK) where
    Z :: (n ~ ZK) => Nat ZK
    S :: Nat n -> Nat (SK n)

class GetNat (n::NatK) where
    getNat :: Nat n

instance GetNat ZK where
    getNat = Z

instance (GetNat n) => (GetNat (SK n)) where
    getNat = S getNat




type family K a where
    K (Nat a) = a

data Vector a (n::NatK) where
    Nil :: Vector a ZK
    Cons :: a -> Vector a b -> Vector a (SK b)

deriving instance (Show a) => Show (Vector a n)

newtype Matrix a (m::NatK) (n::NatK) = Matrix {unMatrix :: (Vector (Vector a m) n)} deriving Show

type ColumnVector a (n::NatK) = Matrix a (SK ZK) n

type RowVector a (n::NatK) = Matrix a n (SK ZK)

type SquareMatrix a (n::NatK) = Matrix a n n

natTimes :: Nat v -> (forall i. t (i::NatK) -> t (SK i)) -> (t ZK -> t v)
natTimes Z _ a = a
natTimes (S n) f a = f (natTimes n f a)

zeroByN :: Nat n -> Matrix a ZK n
zeroByN n = natTimes n zeroBySucc (Matrix Nil) where
    zeroBySucc :: Matrix a ZK i -> Matrix a ZK (SK i)
    zeroBySucc (Matrix v) = (Matrix (Cons Nil v))

zeroVec :: (Num a) => Nat n -> Vector a n
zeroVec Z = Nil
zeroVec (S n) = Cons 0 (zeroVec n)

prependRow :: Vector a m -> Matrix a n m -> Matrix a (SK n) m
prependRow Nil (Matrix Nil) = Matrix Nil
prependRow (Cons a as) (Matrix (Cons v vs)) = Matrix (Cons (Cons a v) (unMatrix (prependRow as (Matrix vs))))

prependColumn :: Vector a m -> Matrix a m n -> Matrix a m (SK n)
prependColumn v (Matrix vs) = Matrix (Cons v vs)

transpose :: (GetNat m) => Matrix a m n -> Matrix a n m
transpose (Matrix Nil) = zeroByN getNat
transpose (Matrix (Cons v vs)) = prependRow v (transpose (Matrix vs))     

vAdd :: (Num a) => Vector a n -> Vector a n -> Vector a n
vAdd Nil Nil = Nil
vAdd (Cons a as) (Cons b bs) = Cons (a+b) (vAdd as bs)

mAdd :: (Num a) => Matrix a m n -> Matrix a m n -> Matrix a m n
mAdd (Matrix Nil) (Matrix Nil) = Matrix Nil
mAdd (Matrix (Cons v vs)) (Matrix (Cons w ws)) = Matrix (Cons (vAdd v w) (unMatrix $ mAdd (Matrix vs) (Matrix ws)))

vFunction :: (forall i. (Less i n) => Nat i -> a) -> Nat n -> Vector a n
vFunction _ Z = Nil
vFunction f (S i) = Cons (f Z) (vFunction (\i' -> f (S i')) i)

mFunction :: (forall i j. (Less i m, Less j n) => Nat i -> Nat j -> a) -> Nat m -> Nat n -> Matrix a m n
mFunction _ Z n = zeroByN n
mFunction _ n Z = transpose (zeroByN n)
--TODO

identityMatrix :: (Num a) => Nat n -> SquareMatrix a n
identityMatrix Z = zeroByN Z
identityMatrix (S n) = prependRow (Cons 1 (zeroVec n)) (prependColumn (zeroVec n) (identityMatrix n))

vDotProduct :: (Num a) => Vector a n -> Vector a n -> a
vDotProduct Nil Nil = 0
vDotProduct (Cons a v) (Cons b w) = (a*b) + vDotProduct v w


vGet :: (Less i n) => Nat i -> Vector a n -> a
vGet Z (Cons a _) = a
vGet (S i) (Cons _ as) = vGet i as

mGetColumn :: (Less i n) => Nat i -> Matrix a m n -> Vector a m
mGetColumn i (Matrix vs) = vGet i vs

mGetRow :: (Less i m, GetNat m) => Nat i -> Matrix a m n -> Vector a n
mGetRow i m = mGetColumn i (transpose m)



mBlah :: (Num a, Less i m, Less j n, GetNat m) => Nat i -> Nat j -> Matrix a m r -> Matrix a r n -> a
mBlah i j mr rn = vDotProduct (mGetRow i mr) (mGetColumn j rn)

mMult :: (Num a) => Matrix a i j -> Matrix a j k -> Matrix a i k
mMult = undefined

foo = Matrix (Cons (Cons 1 (Cons 0 (Cons 0 Nil))) (Cons (Cons 0 (Cons 1 (Cons 0 Nil))) Nil))

goo = zeroByN (S (S Z))

vectorToList :: Vector a n -> [a]
vectorToList Nil = []
vectorToList (Cons a v) = a : (vectorToList v)

matrixToList :: Matrix a m n -> [[a]]
matrixToList (Matrix vs) = map (vectorToList) (vectorToList vs)

main = do
    print "["
    print $ matrixToList foo
    print $ matrixToList $ transpose foo
    print $ matrixToList $ identityMatrix (S (S (S (S Z))))
    print "]"