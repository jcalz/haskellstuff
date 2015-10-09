{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Isomorphism where

import Data.List
import Data.Typeable

class Iso i where
    type X i
    type Y i 
    xTy :: i -> X i -> Y i
    yTx :: i -> Y i -> X i
    isoName :: i -> String
    
    xaTya :: i -> (X i ->a) -> (Y i ->a)
    xaTya i  = ( . (yTx i) )

    axTay :: i -> (a -> X i) -> (a -> Y i)
    axTay i = ( (xTy i) . )

    xxTyy :: i -> (X i -> X i) -> (Y i -> Y i)
    xxTyy i = (axTay i) . (xaTya i) 

    comm :: i -> IsoComm i
    comm = IsoComm

    yaTxa :: i -> (Y i -> a) -> (X i -> a)    
    yaTxa = xaTya . comm

    ayTax :: i -> (a -> Y i) -> (a -> X i)
    ayTax = axTay . comm

    yyTxx :: i -> (Y i -> Y i) -> (X i -> X i)
    yyTxx = xxTyy . comm

    abxTaby :: i -> (a -> b -> X i) -> (a -> b -> Y i)
    abxTaby i =  ((.) . (.)) (xTy i) 

    axbTayb :: i -> (a -> X i -> b) -> (a -> Y i -> b)
    axbTayb i = ((xaTya i) . )

    xabTyab :: i -> (X i -> a -> b) -> (Y i -> a -> b)
    xabTyab i = xaTya i

    axxTayy :: i -> (a -> X i -> X i) -> (a -> Y i -> Y i)
    axxTayy i = (abxTaby i) . (axbTayb i)

    xaxTyay :: i -> (X i -> a -> X i) -> (Y i -> a -> Y i)
    xaxTyay i = (abxTaby i) . (xabTyab i)

    xxaTyya :: i -> (X i -> X i -> a) -> (Y i -> Y i -> a)
    xxaTyya i = (xabTyab i) . (axbTayb i)

    xxxTyyy :: i -> (X i -> X i -> X i) -> (Y i -> Y i -> Y i)
    xxxTyyy i = (abxTaby i) . (axbTayb i) . (xabTyab i)

    trans ::  (Iso j, Y i ~ X j) => i -> j -> (IsoTrans i j)
    trans i j = IsoTrans i j

data IsoTrans i j = IsoTrans i j
instance (Iso i, Iso j, Y i ~ X j) => Iso (IsoTrans i j) where
    type X (IsoTrans i j) = X i
    type Y (IsoTrans i j) = Y j
    xTy (IsoTrans i j) = (xTy j) . (xTy i)
    yTx (IsoTrans i j) = (yTx i) . (yTx j)
    isoName (IsoTrans (i::i) (j::j)) = "{"++(isoName (undefined::i))++" -> "++(isoName (undefined::j))++"}"

data IsoComm i = IsoComm i
instance (Iso i) => Iso (IsoComm i) where
    type X (IsoComm i) = Y i
    type Y (IsoComm i) = X i
    xTy (IsoComm i) = yTx i
    yTx (IsoComm i) = xTy i
    isoName (IsoComm (i::i)) = "{ Inverse "++(isoName (undefined::i))++" }"

data Reflexive x
instance Iso (Reflexive x) where
    type X (Reflexive x) = x
    type Y (Reflexive x) = x
    xTy _ = id
    yTx _ = id
    isoName _ = "Reflexive"

---

class Transformable t where
    transform :: (Iso i) => i -> t (X i) -> t (Y i)

    unTransform :: (Iso i) => i -> t (Y i) -> t (X i)
    unTransform = transform . comm

---




----

