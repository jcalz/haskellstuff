{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OtherMap where

class (Eq (K m)) => Map m where
    type K m :: * 
    type V m :: *
    get :: K m -> m -> Maybe (V m)
    put :: K m -> V m -> m -> m
    remove :: K m -> m -> m
    entries :: m -> [(K m,V m)]
    --mapKeys :: (K m -> k') -> m -> M m k' v

instance (Eq k) => Map ([(k,v)]) where
    type K [(k,v)] = k
    type V [(k,v)] = v
    get k [] = Nothing
    get k ((k',v):es)
        | k==k' = Just v
        | otherwise = get k es
    put k v es = (k,v):es
    remove k es = filter (\(k',v) -> k/=k') es
    entries = id

--instance (Eq k) => Map [(,)] k v        