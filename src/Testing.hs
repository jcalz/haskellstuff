{-# LANGUAGE TypeFamilies #-}
module Testing where

import Map
import Isomorphism
import qualified Set as S

data AssocPair a b c
instance Iso (AssocPair a b c) where
    type X (AssocPair a b c) = (a,(b,c))
    type Y (AssocPair a b c) = ((a,b),c)
    xTy _ (a,(b,c)) = ((a,b),c)
    yTx _ ((a,b),c) = (a,(b,c))
    isoName _ = "AssocPair"

data UnnestedTriplet a b c
instance Iso (UnnestedTriplet a b c) where
    type X (UnnestedTriplet a b c) = (a,(b,c))
    type Y (UnnestedTriplet a b c) = (a,b,c)
    xTy _ (a,(b,c)) = (a,b,c)
    yTx _ (a,b,c) = (a,(b,c))
    isoName _ = "Unnested Triplet"


intmap = AssocList [(1,"Cat"),(2,"Dog"),(3,"Pig")]

data IntegerToParityPair
instance Iso (IntegerToParityPair) where
    type X (IntegerToParityPair) = Integer
    type Y (IntegerToParityPair) = (Integer, Bool)
    xTy _ i = (i `div` 2, i `mod` 2 == 1)
    yTx _ (i, p) = 2*i + (if p then 1 else 0)
    isoName _ = "IntegerToParityPair"

othermap = (tmk (undefined::IntegerToParityPair) intmap) 

--rev = comm (undefined::IntegerToParityPair)

firstmap = (tmk (comm (undefined::IntegerToParityPair)) othermap)

blonk = (tmk (undefined::IntegerToParityPair) (emptyMap::(AssocList Integer String)))    
blonk1 = put (3,False) "arg" blonk

glonk = (tmk (comm (undefined::IntegerToParityPair)) blonk1)

setA = [1,2,5,9,11,6]
setB = [6,10,11,15,30,2,6]

main = do
 print "start"
 --print $ Map.entries $ S.unMapSet $ S.add 4 $ S.MapSet glonk ""
 print $ setB S.∪ setA
 print "end"