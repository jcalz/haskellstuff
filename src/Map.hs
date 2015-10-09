{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
module Map where

import Isomorphism
import Data.List

class Map m where
    type K m
    type V m
    --type MT m :: * -> * -> *
    emptyMap :: m
    get :: (K m) -> m -> Maybe (V m)
    put :: (K m) -> (V m) -> m -> m
    containsKey :: (K m) -> m -> Bool
    remove :: (K m) -> m -> m
    containsValue :: (V m) -> m -> Bool
    entries :: m -> [(K m, V m)]
    keys :: m -> [(K m)]
    keys = (map fst) . entries
    values :: m -> [(V m)]
    values = (map snd) . entries
    putEntry :: (K m, V m) -> m -> m
    putEntry (k,v) = put k v
    putEntries :: [(K m, V m)] -> m -> m
    putEntries = flip (foldr putEntry)
    fromEntries :: [(K m, V m)] -> m
    fromEntries = (flip putEntries) emptyMap 
    

data MapWrap k v = forall m. (Map m, K m ~ k, V m ~ v) => MapWrap m 
instance (Eq k, Eq v) => Map (MapWrap k v) where
    type K (MapWrap k v) = k
    type V (MapWrap k v) = v
    --type MT (MapWrap k v) = MapWrap
    emptyMap = MapWrap (AssocList [])
    get k (MapWrap m) = get k m
    put k v (MapWrap m) = MapWrap (put k v m)
    containsKey k (MapWrap m) = containsKey k m
    remove k (MapWrap m) = MapWrap (remove k m)
    containsValue v (MapWrap m) = containsValue v m
    entries (MapWrap m) = entries m

maybeToBool :: Maybe v -> Bool
maybeToBool Nothing = False
maybeToBool _ = True

data AssocList k v = AssocList [(k,v)] deriving Show
instance (Eq k, Eq v) => Map (AssocList k v) where
    type K (AssocList k v) = k
    type V (AssocList k v) = v
    --type MT (AssocList k v) = AssocList
    emptyMap = AssocList []
    get k (AssocList kvs) = fmap snd (find ((== k).fst) kvs)
    put k v (AssocList kvs) = AssocList ((k,v):kvs)
    containsKey k = maybeToBool . (get k)
    remove k (AssocList kvs) = AssocList $ filter ((/= k).fst) kvs    
    containsValue v (AssocList kvs) = maybeToBool (find ((== v).snd) kvs)
    entries (AssocList kvs) = kvs


---

data TransformMapKey i m = TransformMapKey i m deriving Show

--newtype TMKMT i m k v = TransformMapKey i (MT m k v)

instance (Iso i, Map m, X i ~ K m, Eq (Y i)) => Map (TransformMapKey i m) where
    type K (TransformMapKey i m) = Y i
    type V (TransformMapKey i m) = V m
 --   type MT (TransformMapKey i m) = 
    emptyMap = (TransformMapKey undefined emptyMap)
    get k (TransformMapKey i m) = get (yTx i k) m
    put k v (TransformMapKey i m) = TransformMapKey i (put (yTx i k) v m)
    containsKey k (TransformMapKey i m) = containsKey (yTx i k) m
    remove k (TransformMapKey i m) = TransformMapKey i (remove (yTx i k) m)
    containsValue v (TransformMapKey i m) = containsValue v m
    entries (TransformMapKey i m) = map (\(k,v) -> (xTy i k,v)) (entries m)

class (Iso i, Map m, K m ~ X i) => TMK i m where
    tmk :: i -> m -> MapWrap (Y i) (V m)

instance (Iso i, Map m, K m ~ X i, Eq (Y i)) => TMK i m where
    tmk i m = MapWrap (TransformMapKey i m)

instance (Iso i, Iso j, Map m, K m ~ X j, X i ~ Y j, Eq (Y j), Eq (Y i)) => TMK i (TransformMapKey j m) where 
    tmk i (TransformMapKey j m) = MapWrap $ TransformMapKey (trans j i) m


---

data TransformMapValue i m = TransformMapValue i m deriving Show

instance (Iso i, Map m, X i ~ V m) => Map (TransformMapValue i m) where
    type K (TransformMapValue i m) = K m
    type V (TransformMapValue i m) = Y i
    emptyMap = (TransformMapValue undefined emptyMap)
    get k (TransformMapValue i m) = fmap (xTy i) (get k m)
    put k v (TransformMapValue i m) = TransformMapValue i (put k (yTx i v) m)
    containsKey k (TransformMapValue i m) = containsKey k m
    remove k (TransformMapValue i m) = TransformMapValue i (remove k m)
    containsValue v (TransformMapValue i m) = containsValue (yTx i v) m
    entries (TransformMapValue i m) = map (\(k,v) -> (k,xTy i v)) (entries m)
    

class (Iso i, Map m, V m ~ X i) => TMV i m where
    tmv :: i -> m -> MapWrap (K m) (Y i)

instance (Iso i, Map m, V m ~ X i) => TMV i m where
    tmv i m = MapWrap (TransformMapValue i m)

instance (Iso i, Iso j, Map m, V m ~ X j, X i ~ Y j, Eq (Y j), Eq (Y i)) => TMV i (TransformMapValue j m) where 
    tmv i (TransformMapValue j m) = MapWrap $ TransformMapValue (trans j i) m
