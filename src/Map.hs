{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Map where

import qualified Data.List as L
import qualified Data.Function as F

mySortBy f = L.sortBy (\a b -> (f a) `compare` (f b))


data Entry k v = Entry {getKey::k, getValue::v} deriving Eq
instance (Show k, Show v) => Show (Entry k v) where
    show (Entry k v) = "{" ++ (show k) ++ "=>" ++ (show v) ++ "}"


class (Eq k) => Map m k v | m -> k, m -> v where
    
    get :: m -> k -> Maybe v
    entries :: m -> [Entry k v]

    contains :: m -> k -> Bool
    contains m k = toBool (get m k) where
        toBool Nothing = False
        toBool _ = True

    keys :: m -> [k]
    keys = (map getKey) . entries

    values :: m -> [v]
    values = (map getValue) . entries

    empty :: m
   
    put :: m -> k -> v -> m    
    remove :: m -> k -> m
            
    putAll :: m -> [Entry k v] -> m
    putAll = foldr (\(Entry k v) acc -> put acc k v) 

    removeAll :: m -> [k] -> m
    removeAll = foldr (flip remove)

    initialize :: [Entry k v] -> m
    initialize = putAll empty

    reinitialize :: m -> m
    reinitialize = initialize . entries

data Eq k => LinearMap k v = LinearMap [Entry k v] deriving Show



instance Eq k => Map (LinearMap k v) k v where
   
    empty = LinearMap []

    get (LinearMap kvs) k = case L.find (\(Entry k' _) -> k'==k) kvs of 
        Nothing -> Nothing 
        Just (Entry _ v) -> Just v

    entries (LinearMap kvs) =  kvs

    remove (LinearMap kvs) k = LinearMap (L.filter (\(Entry k' _)-> k'/=k) kvs)

    put m@(LinearMap kvs) k v = let LinearMap removed = remove m k in
                                          LinearMap (Entry k v  : removed)







data (Map m k v) => PartMap m k v = PartMap m k

(~~) :: (Map m k v) => m -> k -> PartMap m k v
m ~~ k = PartMap m k

(~>) :: (Map m k v) => PartMap m k v -> v -> m
(PartMap m k) ~> v = put m k v


data LayeredMap dm sm k v= LayeredMap dm sm deriving Show

instance (Map dm k v, Map sm k (Maybe v), Eq k) => Map (LayeredMap dm sm k v) k v where
    empty = LayeredMap empty empty
    remove (LayeredMap dm sm) k = LayeredMap dm (put sm k Nothing)
    put (LayeredMap dm sm) k v = LayeredMap dm (put sm k (Just v))
    get (LayeredMap dm sm) k = case get sm k of
        Just (Just v) -> Just v
        _ -> Nothing
    entries (LayeredMap dm sm) = let ds = entries dm
                                     ss = entries sm
                                     ks = keys sm
                                     in
                                     [Entry k v | Entry k v <- ds, not (elem k ks)] ++ [Entry k v | Entry k (Just v) <- ss]

    initialize es = LayeredMap (initialize es) empty




data ReadOnlyTreeMap k v = Node (Entry k v) (ReadOnlyTreeMap k v) (ReadOnlyTreeMap k v) | Empty deriving Show

instance (Ord k) => Map (ReadOnlyTreeMap k v) k v where
    remove = undefined
    put = undefined
    empty = Empty

    get Empty _ = Nothing
    get (Node (Entry kt v) lt rt) k 
        | (k == kt) = Just v
        | (k < kt) = get lt k
        | (k > kt) = get rt k

    entries Empty = []
    entries (Node e l r) = (entries l) ++ (e : entries r)

    

    initialize es = initializeSorted (mySortBy getKey es) where
        initializeSorted [] = Empty
        initializeSorted es =  Node n (initializeSorted l) (initializeSorted r) where 
                                (l, (n:r)) = splitAt ((length es) `div` 2) es 

--data DepthTree a = Value a | DNode a a 

--dget :: DepthTree a -> [Bool] -> a
--dget (Value a) _ = a
--dget (DNode l@(Value _) _) (False:bs) = dget l bs



myMap = initialize [Entry "dog" 3, Entry "cat" 7, Entry "rat" 4, Entry "pig" 1] :: LinearMap String Integer

yourMap = initialize [Entry "dog" 3, Entry "cat" 7, Entry "rat" 4, Entry "pig" 1] :: ReadOnlyTreeMap String Integer


makeLayeredMap :: (Ord k) => [Entry k v] -> LayeredMap (ReadOnlyTreeMap k v) (LinearMap k (Maybe v)) k v
makeLayeredMap = initialize


m0 = makeLayeredMap [Entry "dog" 3, Entry "cat" 7, Entry "rat" 4, Entry "pig" 1] 
m1 = put m0 "cow" 15
m2 = put m1 "monkey" 3
m3 = reinitialize m2

sq0 = makeLayeredMap $ take 100 [(Entry i (i*i)) | i <- [1..]]
sq1 = removeAll sq0 [7..1000]
sq2 = reinitialize sq1





main = do
    print "start"
    print $ sq1
    print $ sq2
    print "done"
