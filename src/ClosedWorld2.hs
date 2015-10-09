{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}
module ClosedWorld2 where

data Z = Z
newtype S n = S n

newtype I x = I { unI :: x }
newtype K x y = K { unK :: x }

class Nat n where
  caseNat :: forall r. n -> (n ~ Z => r) -> (forall p. (n ~ S p, Nat p) => p -> r) -> r

instance Nat Z where
    caseNat _ z _ = z
instance Nat n => Nat (S n) where
    caseNat (S n) _ s = s n

induction :: forall p n. Nat n => n -> p Z -> (forall x. Nat x => p x -> p (S x)) -> p n
induction n z s = caseNat n isZ isS where
    isZ :: n ~ Z => p n
    isZ = z
    isS :: forall x. (n ~ S x, Nat x) => x -> p n
    isS x = s (induction x z s)

witnessNat :: forall n. Nat n => n
witnessNat = theWitness where
   theWitness = unI $ induction (undefined `asTypeOf` theWitness) (I Z) (I . S . unI)

data AnyNat where AnyNat :: Nat n => n -> AnyNat 

instance Show (AnyNat) where
    show (AnyNat n) = show (natToInteger n)
                  
data IsNat n where IsNat :: Nat n => IsNat n 

deriving instance Show (IsNat n) 

mkNat :: Integer -> AnyNat
mkNat x | x < 0 = error "Nat must be >= 0"
mkNat 0 = AnyNat Z
mkNat x = case (mkNat (x-1)) of (AnyNat n) -> AnyNat (S n)

natToInteger :: Nat n => n -> Integer
natToInteger n = unK $ induction n (K 0) (K . (+1) . unK)

data TEq a b where TEq :: (a ~ b) => TEq a b



data Vec a n = (n ~ Z)=>Nil | forall m. (Nat m, Nat n, n ~ (S m)) => Cons a (Vec a m)

headVec :: Vec a (S n) -> a
headVec (Cons a _) = a

tailVec :: Vec a (S n) -> Vec a n
tailVec (Cons _ v) = v
 

instance (Show a) => Show (Vec a n) where
    show v = "< "++ subshow v ++ " >" where
        subshow :: Vec a n' -> String
        subshow Nil = ""
        subshow (Cons a Nil) = show a
        subshow (Cons a v) = show a ++ ", " ++ subshow v

myReplicate :: (Nat n) => a -> Vec a n
myReplicate a = induction witnessNat Nil (Cons a)                   
                                
foo = (myReplicate 3) :: (Vec Integer (S (S (S Z))))
    
fold :: Nat n => (forall x. Nat x => a -> p x -> p (S x)) -> p Z -> Vec a n -> p n
fold _ z (Nil) = z 
fold f z (Cons a v) = f a (fold f z v)

fold1 :: forall p a pn . (Nat pn) => (forall x. Nat x => a -> p x -> p (S x)) -> (a -> p (S Z)) -> Vec a (S pn) -> p (S pn)
fold1 f z = caseNat (undefined::pn) (\(Cons a Nil) -> z a) (\pn (Cons a v) -> f a (fold1 f z v))

mapVec :: Nat n => (a -> b) -> (Vec a n) -> (Vec b n)
mapVec f Nil = Nil
mapVec f (Cons a v) = (Cons (f a) $ mapVec f v)

-- let's do MAX, where p n = Integer

data SimpleWrap a n = SimpleWrap {unwrap :: a}
  
simpleFold :: Nat n => (a -> b -> b) -> b -> Vec a n -> b
simpleFold f z = unwrap . fold (\a (SimpleWrap b) -> SimpleWrap (f a b)) (SimpleWrap z) 

simpleFold1 :: Nat pn => (a -> a -> a) -> Vec a (S pn) -> a
simpleFold1 f = unwrap . fold1 (\a (SimpleWrap b) -> SimpleWrap (f a b)) (\a -> SimpleWrap a)

sumVec :: (Nat n, Num a) => Vec a n -> a
sumVec = simpleFold (+) 0

maxVec :: (Nat n, Ord a) => Vec a (S n) -> a
maxVec = simpleFold1 (max)


--Prove that all sized lists have a length that is a natural number:
lengthIsNat :: Vec a n -> IsNat n
lengthIsNat Nil = IsNat
lengthIsNat (Cons a v) = IsNat

assureLengthIsNat :: forall n n' a . Vec a n -> ((Nat n) => Vec a n)
assureLengthIsNat v = (const v) (lengthIsNat v)

succEq :: (Nat x, Nat y) => Maybe (TEq x y) -> Maybe (TEq (S x) (S y))
succEq Nothing = Nothing
succEq (Just TEq) = (Just TEq)

natEqDec :: forall x y. (Nat x, Nat y) => x -> y -> Maybe (TEq x y)
natEqDec x y = caseNat x (caseNat y (Just TEq) (const Nothing)) (caseNat y (const Nothing) (\py px -> succEq $ natEqDec px py))

type family x :+ y where 
    Z :+ y = y
    (S x) :+ y = S(x :+ y)


plusIsNat :: forall x y.(Nat x, Nat y) => x -> y -> IsNat (x :+ y)
plusIsNat x y = caseNat x (IsNat) (\p -> inc $ plusIsNat p y) where
    inc :: IsNat x' -> IsNat (S x')
    inc IsNat = IsNat

--assurePlusIsNat :: forall a x y. (Nat x, Nat y) => x -> y -> a -> (Nat (x:+y) => a)
--assurePlusIsNat x y a = caseNat x (a) (\px -> inc $ assurePlusIsNat px y a) where
  --  inc :: (Nat x') => ((Nat x')=>a) -> (Nat (S x') => a)
    --inc a = a 

-- x + 0 = 0 + x
xPlusZero :: forall x. (Nat x) => x -> TEq (x :+ Z) (Z :+ x)
xPlusZero x = caseNat x (TEq) (\px -> inc $ xPlusZero px) where
    inc :: TEq (px :+ Z) (Z :+ px) -> TEq ((S px) :+ Z) (Z :+ (S px))
    inc TEq = TEq

-- x + Sy = S(x+y)
succRight :: forall x y. (Nat x, Nat y) => x -> y ->  TEq (x :+ (S y)) (S (x :+ y))
succRight x y = caseNat x (TEq) (\px -> inc px y $ succRight px y) where   
    inc :: px -> y -> TEq (px :+ S y) (S (px :+ y)) -> TEq ((S px) :+ (S y)) (S ((S px) :+ y))
    inc px y TEq = TEq

-- x + y = y + x  implies  x + Sy = Sy + x
commToSuccComm :: forall x y. (Nat x, Nat y) => x -> y -> TEq (x :+ y) (y :+ x) -> TEq (x :+ (S y)) (S y :+ x)
commToSuccComm x y TEq = succRight x y

-- x + y = y + x !!
plusIsComm :: forall x y. (Nat x, Nat y) => x -> y -> TEq (x :+ y) (y :+ x)
plusIsComm x y = caseNat y (xPlusZero x) (\py -> commToSuccComm x py $ plusIsComm x py)


data VecPlus a n m = (Nat n, Nat m, Nat (m:+n)) => VecPlus { uvp :: Vec a (m:+n)}
append ::  (Nat m, Nat n) => Vec a m -> Vec a n -> Vec a (m :+ n)
append v w = uvp $ fold (\a (VecPlus vw) -> VecPlus (Cons a vw)) (VecPlus w) v 


reverseVec :: Vec a n -> Vec a n
reverseVec Nil = Nil
reverseVec (Cons a v) = transformWitness (addingOneIsSuccession witnessNat) reversedVec where    
    reversedVec = (append (reverseVec v) (Cons a Nil))     
    addingOneIsSuccession x = transitiveTEq (succRight x Z) (xPlusZero (S x))

transformWitness :: TEq m n -> p m -> p n
transformWitness TEq = id

transitiveTEq :: TEq a b -> TEq b c -> TEq a c
transitiveTEq TEq TEq = TEq

commutativeTEq :: TEq a b -> TEq b a
commutativeTEq TEq = TEq



instance Eq AnyNat where
 (AnyNat x) == (AnyNat y) = isSomething (natEqDec x y) where
     isSomething :: Maybe a -> Bool
     isSomething Nothing = False
     isSomething _ = True

instance Num AnyNat where
    (AnyNat x) + (AnyNat y) = mkNat (natToInteger x + natToInteger y)
    (AnyNat x) * (AnyNat y) = mkNat (natToInteger x * natToInteger y)
    abs a = a
    signum (AnyNat a) = mkNat (signum (natToInteger a))
    fromInteger = mkNat
    (AnyNat x) - (AnyNat y) = mkNat (natToInteger x - natToInteger y)

type family x :* y where 
    Z :* y = Z
    (S x) :* y = (x :* y) :+ y 

multIsNat :: forall x y.(Nat x, Nat y) => x -> y -> IsNat (x :* y)
multIsNat x y = caseNat x (IsNat) (\p -> inc IsNat $ multIsNat p y) where
    inc :: IsNat y -> IsNat py -> IsNat (py :+ y)
    inc (IsNat::IsNat y) (IsNat::IsNat py) = plusIsNat (witnessNat::py) (witnessNat::y)

assureIsNat :: IsNat n -> a -> (Nat n => a)
assureIsNat IsNat = id

--assureMultIsNat :: forall a x y. (Nat x, Nat y) => x -> y -> a -> (Nat (x:*y) => a)
--assureMultIsNat x y a = caseNat x (a) (\px -> assureMultIsNat px y a) 

gloook = assureLengthIsNat (undefined :: Vec Bool Z)

--append' :: forall a m n. (Nat m, Nat n) => Vec a m -> Vec a n -> (Nat (m:+n) => Vec a (m :+ n))
--append' Nil w = w
--append' (Cons a (v::Vec a pm)) (w::Vec a n) = Cons a (undefined v w)

--cartesianProduct :: forall a b m n . (Nat m, Nat n) => Vec a m -> Vec b n -> Vec (a,b) (m :* n)
--cartesianProduct Nil _ = Nil
--cartesianProduct (Cons a (v::Vec a pm)) w = append subProduct mapped where
  --  mapped = (mapVec (\b->(a,b)) w) 
    --subProduct = assureLengthIsNat $ glarp 
    --glarp = (undefined (witnessNat::pm) (witnessNat::n)) (cartesianProduct v w)
    

--data VecMult a n m = (Nat n, Nat m, Nat (m:*n)) => VecMult { um :: m, un :: n, uvm :: Vec a (m:*n)}
--cartesianProduct ::  (Nat m, Nat n) => Vec a m -> Vec b n -> Vec (a,b) (m :* n)
--cartesianProduct v w = uvm $ fold (\a (VecMult m n vw) -> VecMult (S m) n (append vw (mapVec (\b->(a,b)) w) )) (VecMult Z witnessNat Nil) v 


zipVec :: Vec a n -> Vec b n -> Vec (a,b) n
zipVec Nil _ = Nil
zipVec (Cons a v) (Cons b w) = Cons (a,b) (zipVec v w)


needCommutativeZip :: forall a m n. (Nat m, Nat n) =>  Vec a m -> Vec a n -> Vec (a,a) (m :+ n)
needCommutativeZip v w = zipVec (append v w) (transformWitness (plusIsComm (witnessNat::n) (witnessNat::m)) (append w v))


va = Cons 1 $ Cons 2 $ Cons 3 Nil
vb = Cons 8 $ Cons 9 $ Cons 10 $ Cons 11 Nil
vab = append va vb
vba = append vb va

main = do
    print "start"
    print $ needCommutativeZip va vb
    print "end"



