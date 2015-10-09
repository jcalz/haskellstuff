{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module ClosedWorld where

data Zero
data Succ n
 
class Nat n where
   switch ::
      f Zero ->
      (forall m. Nat m => f (Succ m)) ->
      f n

 
instance Nat Zero where
   switch x _ = x
 
instance Nat n => Nat (Succ n) where
   switch _ x = x




type family Add n m :: *
type instance Add Zero m = m
type instance Add (Succ n) m = Succ (Add n m)
 
data Vec n a where
    Nil :: Vec Zero a
    Cons :: a -> Vec n a -> Vec (Succ n) a


decons :: Vec (Succ num) a -> (a, Vec num a)
decons (Cons a v) = (a,v)
 
cons :: a -> Vec n a -> Vec (Succ n) a
cons a v = Cons a v

newtype
   Append m a n =
      Append {runAppend :: Vec n a -> Vec m a -> Vec (Add n m) a}
 
append :: Nat n => Vec n a -> Vec m a -> Vec (Add n m) a
append =
   runAppend $
   switch
      (Append $ \_empty x -> x)
      (Append $ \x y ->
          case decons x of
             (a,as) -> cons a (append as y))


-- wants (Vec a (m :+ S Z) ~ Vec a (S m))

newtype 
    AddOne a n =
        AddOne {runAddOne :: Vec (Add n (Succ Zero)) a -> Vec (Succ n) a }

addOne :: Nat n => Vec (Add n (Succ Zero)) a -> Vec (Succ n) a
addOne =
    runAddOne $
    switch
        (AddOne id)
        (AddOne $ \v ->
            case decons v of
                (a, as) -> cons a (addOne as))

newtype
  Reverse a n =
      Reverse {runReverse :: Vec n a -> Vec n a}

vReverse :: Nat n => Vec n a -> Vec n a
vReverse =
    runReverse $
    switch
        (Reverse $ \n -> n)
        (Reverse $ \v ->
            case decons v of
                (a, as) -> addOne $ append (vReverse as) (cons a Nil))

vZip :: Vec n a -> Vec n b -> Vec n (a,b)
vZip Nil _ = Nil
vZip (Cons a as) (Cons b bs) = Cons (a,b) (vZip as bs)

vWacky :: (Nat n) => Vec n a -> Vec n (a,a)
vWacky v = vZip v (vReverse v)

newtype
 NNum n =
     NNum {runNNum :: n -> Integer}

nNum :: Nat n => n -> Integer
nNum =
    runNNum $
        switch
        (NNum $ const 0)
        (NNum $ \(x::Succ m) -> 1 + nNum (undefined::m))



glop = vWacky (Cons 1 (Cons 2 (Cons 3 Nil)))


show' :: (Show a) => Vec n a -> String
show' Nil = ""
show' (Cons a Nil) = (show a)
show' (Cons a v') = (show a)++", "++(show' v')

instance (Show a, Nat n) => Show (Vec n a) where
    show v = "Vec "++show (nNum (undefined::n)) ++": {"++(show' v)++"}"


main = putStrLn (show glop)
