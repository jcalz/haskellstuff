{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes, OverlappingInstances #-}
module Test2 where

newtype Pair a b=Pair (forall c.(a->b->c)->c)

data Z = Z
data S a = S a
data P a = P a

class Zero z
instance Zero Z

class Pos p
instance Pos (S Z)
instance (Pos p) => Pos (S p)

class Neg n
instance Neg (P Z)
instance (Neg n) => Neg (P n)


foo = Pair (\f -> f 'a' 3)

frs (Pair d) = d (\x y -> x)
sec (Pair d) = d (\x y -> y)

blark = frs foo
blark' = sec foo

data NumWrap = forall a. (Num a, Show a) => NumWrap a 

instance Show (NumWrap) where
    show (NumWrap n) = show n

hoo :: [NumWrap]
hoo = [NumWrap (7::Int), NumWrap (5::Integer), NumWrap 3.0, NumWrap (6::Rational)]


blop :: NumWrap -> NumWrap
blop (NumWrap n) = (NumWrap (n*2))

main = do
    print "start"
    print hoo
    print $ map blop hoo
    print "end"