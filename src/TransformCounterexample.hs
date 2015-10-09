{-# LANGUAGE TypeFamilies #-}
module TransformCounterexample where


type family DePair a where
    DePair (a,b) = a
    DePair a = a

class Isomorphic i where
    type X i
    type Y i
    xy :: i -> X i -> Y i
    yx :: i -> Y i -> X i

data CounterExample a = CounterExample a (DePair a)

foo = CounterExample 'a' 'a'

goo = CounterExample ('a','b') 'c'

data Pair a b = Pair a b

data TransformPair a b
instance (Isomorphic (TransformPair a b)) where
    type X (TransformPair a b) = (a,b)
    type Y (TransformPair a b) = Pair a b
    xy _ (a,b) = Pair a b
    yx _ (Pair a b) = (a,b)

class Transformable t where
    transform :: (Isomorphic i) => i -> t (X i) -> t (Y i)

data Example a = Example a [a] (a -> Bool)

instance Transformable Example where
    transform i (Example a as pred) = Example (xy i a) (map (xy i) as) (pred.(yx i))

instance Transformable CounterExample where
    transform i (CounterExample a b) = CounterExample (xy i a) undefined
-- Can't do this, because even if x is isomorphic to y, DePair x may not be isomorphic to DePair y
-- Pair a b is isomorphic to (a,b) but DePair (Pair a b) ~ Pair a b is not isomorphic to DePair (a,b) ~ a


-- let's call a type function t FullyInjective if (t x) ~ (t y) implies (x ~ y), AND 
    -- if all type functions used in the definition of t are FullyInjective
-- it seems that all FullyInjective type functions should be Transformable.  
-- but since Haskell probably can't determine this, you need to write your own implementations of Transformable
--  ,,, maybe there's a way to have something "deriving" Transformable



     
