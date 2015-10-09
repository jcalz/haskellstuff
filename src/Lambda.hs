module Lambda where

import Prelude hiding (succ, exp)

data L = L {display :: String, l :: L -> L}

instance Show L where
    show = display

ident = L ("ident") id
constant = L ("constant") (\x -> L ("const("++(display x)++")") (const x))

zero = L "0" (const one)
one = L "1" id

asInt n = length $ display $ l ((l n) f) z where
    f = L "F" (\x -> L ('c':(display x)) id)
    z = L "" id

fromInt :: Int -> L
fromInt n 
    | (n <= 0) = zero
    | otherwise = l succ (fromInt (n-1))


succ = L "succ" (\n -> let sn = L (show $ asInt sn) (\f -> L (display f ++ "." ++ display (l n f) ) (\x -> (l f (l (l n f) x)))) in sn)

two = l succ one
three = l succ two
four = l succ three
five = l succ four
six = l succ five
seven = l succ six
eight = l succ seven
nine = l succ eight
ten = l succ nine

add = L "add" (\n -> L ("add("++(display n)++")") (l (l n succ)))

mult = L "mult" (\m -> L ("mult("++(display m)++")") (\n -> let mn = L (show $ asInt mn) (l (l (l m (l add n)) zero)) in mn))

exp = L "exp" (\m -> L ("exp("++(display m)++")") (\n -> let mton = L (show $ asInt mton) (l (l n m)) in mton))

true = L "true" (l constant)
false = L "false" (l (l constant ident))

ifthenelse = L "ifthenelse" id

asBool b = ("" == display (l (l b t) f)) where
    t = L "" id
    f = L "f" id

fromBool :: Bool -> L
fromBool True = true
fromBool False = false

pair = L "pair" (\x -> L ("pair("++(display x)++")") (\y -> L ("pair("++(display x)++","++(display y)++")") (\b -> l (l b x) y)))

first = L "first" (\p -> l p true)
second = L "second" (\p -> l p false)

ll :: L -> L -> L -> L
ll x y z = l (l x y) z

lll :: L -> L -> L -> L -> L
lll w x y z = l (l (l w x) y) z

main = do
    print "start"
    print $ l first (ll pair two three)
    print "end"