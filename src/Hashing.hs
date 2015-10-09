{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Hashing where

import Debug.Trace
import Data.Int

toEnumBound :: (Enum o, Bounded o) => Int -> o
toEnumBound i = o where
    max = fromEnum (maxBound `asTypeOf` o)
    min = fromEnum (minBound `asTypeOf` o)
    range = max - min + 1
    iwrap = if ((i > max) || (i < min)) then
            (((i - min) `mod` range) + min)
            else i
    o = toEnum iwrap
    
class Hashable h where
    hash :: (Enum o, Bounded o) => h -> o
    hash = toEnumBound . hashToInt
    
    hashToInt :: h -> Int

instance (Hashable h) => Hashable [h] where
    hashToInt [] = 0
    hashToInt (h:hs) = (hashToInt h) + 17*(hashToInt hs)

instance (Hashable h, Hashable i) => Hashable (h, i) where
    hashToInt (h,i) = hashToInt h + 17*(hashToInt i)

instance (Hashable h, Hashable i, Hashable j) => Hashable (h,i,j) where
    hashToInt (h,i,j) = hashToInt h + 17*(hashToInt (i,j))

