{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Naturals2 where

data Z = Z
data S n = S n

class Nat n where
 eith:: (f Z) -> (forall m. (Nat m, Nat (S m)) => f (S m) ) -> (f n)
 
instance Nat Z where
  eith z sn = z

instance (Nat n) => Nat (S n) where
  eith z sn = sn

type family m :+ n where
    Z :+ n = n
    (S m) :+ n = S (m:+n)



data (Nat n) => AddZero n = AddZero ((n :+ Z) -> n)
unAddZero (AddZero x) = x

addZero :: (Nat n) => (n:+Z) -> n
addZero = unAddZero $ eith (AddZero id) (AddZero (\(S m) -> (S (addZero m))))

--data (Nat n) => TryAddZero n = TryAddZero (((n :+ Z)~n) => n)
--unTryAddZero (TryAddZero x) = x

--tryAddZero :: (Nat n) => n -> (((n :+ Z) ~ n) => n)
--tryAddZero n = unTryAddZero $ eith (TryAddZero Z) (TryAddZero (S $ tryAddZero undefined))

--goo :: (Nat n) => Maybe (n:+Z) -> Maybe n
--goo (x::(Maybe w)) = tryAddZero (undefined::w)

