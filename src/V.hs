{--
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances, IncoherentInstances #-}

{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, GADTs #-}

module V where

data NatK = ZT | ST NatK


--type family (n :: NatK) :+ (m :: NatK) :: NatK
--type instance ZT :+ m = m
--type instance (ST n) :+ m = ST(n :+ m)

type family (n::NatK) :+ (m::NatK) :: NatK
type instance ZT :+ m = m
type instance (ST n) :+ m = ST (n :+ m)

data Nat n where
    Z :: ((ZT :+ ST ZT) ~ ST (ZT)) => Nat ZT
    S :: ((nn :+ ST ZT) ~ ST (nn)) => Nat nn -> Nat (ST nn)
deriving instance Eq (Nat n)
deriving instance Show (Nat n)

data Vector a n where
  Nil  :: Vector a (Nat ZT)
  (:=) :: ((nnn:+ ST ZT) ~ ST nnn) => a -> Vector a (Nat nnn) -> Vector a (Nat (ST nnn))
infixr 5 :=

append :: Vector a (Nat m) -> Vector a (Nat n) -> Vector a (Nat (m :+ n))
append Nil v = v
append (x := xs) v = x := (append xs v)
(++) = V.append

reverse :: Vector a n -> Vector a n
reverse Nil = Nil
reverse (x:=xs) = V.reverse xs V.++ (x:=Nil)


foo = 3 := 5 := 9 := Nil



main = do
    print $ "hey"
    print $ "done"

    --}