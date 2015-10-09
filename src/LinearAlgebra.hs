{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LinearAlgebra where

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

type family Prev (a::NatK) :: NatK where
    Prev ZK = ZK
    Prev (SK a) = a

type Leq (a::NatK) (b::NatK) = (Minus a b ~ ZK)

type Less (a::NatK) (b::NatK) = (Leq (SK a) b)

data Nat (n :: NatK) where
    Z :: (n ~ ZK) => Nat ZK
    S :: Nat n -> Nat (SK n)

data SomeNat = SomeNat (forall n. Nat n)

asInt :: Nat n -> Integer
asInt Z = 0
asInt (S k) = 1 + asInt k

--prev :: (Nat n -> Nat (Prev n))
--prev (S n) = n
--prev Z = Z


instance Eq SomeNat where
    SomeNat i == SomeNat j = (asInt i == asInt j)

class IsNat (n::NatK) where
    getNat :: Nat n
    uncons :: Vector a n -> Maybe (a, Vector a (Prev n))
    toList :: Vector a n -> [a]
    

instance IsNat ZK where
    getNat = Z
    uncons _ = Nothing    
    toList v = case (uncons v) of 
        Just (h,t) -> h : (toList t)
        Nothing -> []

instance (IsNat n) => (IsNat (SK n)) where
    getNat = S getNat        
    uncons (Getter f) = Just (f Z, Getter (\m -> f (S m)))
    toList v = case (uncons v) of 
        Just (h,t) -> h : (toList t)
        Nothing -> []

--data Vector a (n::NatK) = (n ~ ZK) => Nil | forall m. (n ~ SK m) => Cons a (Vector a m) 

data Vector a (n::NatK) = (n ~ ZK) => NoGet | Getter (forall m. (Less m n) => Nat m -> a)

instance (Show a, IsNat n) => Show (Vector a n) where
    show v = "Vector "++(show $ toList v)




leq :: Nat m -> Nat n -> Bool
leq Z _ = True
leq (S m) Z = False
leq (S m) (S n) = leq m n

less :: Nat m -> Nat n -> Bool
less Z Z = False
less Z _ = True
less (S m) Z = False
less (S m) (S n) = less m n

eq :: Nat m -> Nat n -> Bool
eq m n = leq m n && leq n m

plus :: Nat m -> Nat n -> Nat (Plus m n)
plus Z n = n
plus (S m) n = S (plus m n)

minus :: Nat m -> Nat n -> Nat (Minus m n)
minus Z n = Z
minus m Z = m
minus (S m) (S n) = minus m n

prev :: Nat m -> Nat (Prev m)
prev Z = Z
prev (S m) = m



nil :: Vector a ZK
nil = NoGet

get :: (Less m n) => Nat m -> Vector a n -> a
get m (Getter f) = f m

cons :: a -> Vector a n -> Vector a (SK n)
cons a v = Getter (\m -> case m of 
    Z -> a
    (S n) -> get n v
    )

{--append :: forall a m n. (Plus m ZK ~ m) => Vector a m -> Vector a n -> Vector a (Plus m n)
append NoGet w = w
append v NoGet = v
append (Getter f) (Getter g) = Getter (\k -> 
    if (less k (getNat::Nat m)) then (f k) else (g (k `minus` (getNat::Nat m)))) 
--}

--allNats = iterate f z  where
--    z :: SomeNat
--    z = (SomeNat Z)
--    f :: SomeNat -> SomeNat
--    f = id

v1 = cons 'a' (cons 'b' (cons 'c' nil))

main = do
    print "start"
    print v1
    print "end"

