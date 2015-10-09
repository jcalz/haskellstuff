{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
module Injective where
import GHC.Prim

data Z = Z 
data S n = S n
 


class Nat n where
  induct :: p Z -> (forall m. p m -> p (S m)) -> p n

instance Nat Z where
  induct z _ = z

instance Nat n => Nat (S n) where
  induct z s = s (induct z s)

type family n :+: m
type instance Z   :+: m = m
type instance S n :+: m = S (n :+: m)


--data P n1 n2 m where
--  P :: ((Nat m, forall a. (m :+: n1) ~ (m :+: n2)) => (n1 ~ n2 => a) -> a) -> P n1 n2 m

--injective :: forall m n1 n2 a. ((m :+: n1) ~ (m :+: n2)) => n1 -> n2 -> m -> (n1 ~ n2 => a) -> a
--injective _ _ _ x = case induct (P (\xx -> xx)) (\(P f) -> P f) :: P n1 n2 m of P f -> f x
--injective _ _ _ = f where P f = induct (P (\xx -> xx)) (\(P f) -> P f) 
--injective _ _ _ = case induct (P (\xx -> xx)) (\(P f) -> P f) of P f -> f


--data AddZero n where

--data AddZero n = (Nat n) => AddZero (n :+: Z -> n)

--foo :: (Nat n) => (n :+: Z) -> AddZero n
--foo n =  induct (AddZero id) (\(AddZero f) -> AddZero (\(S m) -> S(f m)))

--goo :: (Nat n) => (n :+: Z) -> n
--goo nz = (f nz) where (AddZero f) = foo nz   

type family AZ n :: Constraint
  where AZ n = (n ~ (n :+: Z))

data X n = (Nat n) => X ((AZ n) => n)

hoo :: (Nat n) => t (n:+:Z) -> ((AZ n) => t n)
hoo x = x -- this should not work like this

data Blop m = (((m :+: (S Z))~(S m))) => Blop m
unblop :: (Blop m) -> (((m :+: (S Z))~(S m))) => m
unblop (Blop m) = m
--data Blop m = (Nat m, ((m :+: (m))~(m))) => Blop m

hmm :: Blop m -> Blop (S m)
hmm (Blop m) = Blop (S m)

j :: (Nat n) => Blop n
j = induct (Blop Z) hmm

data V a n = (n ~ Z) => Nil | forall m.  (n~(S m)) => V a (V a m)

app :: V a m -> V a n -> V a (m :+: n)
app Nil v = v
app (V a v) w = V a (app v w)




grr n = unblop $ Blop n

--whoa :: forall a n. (Nat n) => V a (n :+: (S Z)) -> V a (S n)
--whoa v = undefined v (grr (undefined::n))


rev :: V a n -> V a n
rev Nil = Nil
rev (V a (v::(V a m))) =   undefined $ app (rev v) (V a Nil)