{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Collection where

import qualified Data.Set as Set

class Collection c e | c -> e where
    size :: c -> Integer
    toList :: c -> [e]
    fromList :: [e] -> c
    add :: e -> c -> c
    remove :: (Eq e) => e -> c -> c
    contains :: (Eq e) => e -> c -> Bool

instance Collection [a] a where
    size = toInteger . length
    toList = id
    fromList = id
    contains = elem
    add = (:)
    remove e [] = []
    remove e (x:xs) 
        | x==e = xs
        | otherwise = x: remove e xs

--instance Collection (Set.Set a) a where
  --  size = toInteger . Set.size
    