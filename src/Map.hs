{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Map where

import Control.Monad
 
data Void

type MapEntry k v = (k,v)

class ReadMap m where    
    get :: (Eq k) => k -> m k v -> Maybe v
    
    containsKey :: (Eq k) => k -> m k v -> Bool
    containsKey k = something . (get k) where
        something Nothing = False
        something (Just _) = True

    entries :: (Eq k) => m k v -> [MapEntry k v]

    keys :: (Eq k) => m k v -> [k]
    keys = map fst . entries
    
    values :: (Eq k) => m k v -> [v]
    values = map snd . entries

class (ReadMap m) => Map m where
   
    put :: (Eq k) => k -> v -> m k v -> m k v
   
    putEntry :: (Eq k) => MapEntry k v -> m k v -> m k v
    putEntry (k, v) = put k v

    remove :: (Eq k) => k -> m k v -> m k v

    putEntries :: (Eq k) => [MapEntry k v] -> m k v -> m k v
    putEntries = flip $ foldr putEntry

    removeAll :: (Eq k) => [k] -> m k v -> m k v
    removeAll = flip $ foldr remove

    filterKeys :: (Eq k) => (k -> Bool) -> m k v -> m k v
    filterKeys pred m = removeAll (filter (not . pred) $ keys m) m

    type EmptyMapContext m k v
    
    emptyMap :: (Eq k) => EmptyMapContext m k v -> m k v    

 {--   uncurryMap :: (Eq k, Eq ki, Map mi) => EmptyMapContext m (k,ki) v -> m k (mi ki v) -> m (k,ki) v
    uncurryMap c m = putEntries ( blop $ map (\(k,v)->(k, entries v)) (entries m) ) (emptyMap c) where
        blop :: [(a,[(b,c)])] -> [((a,b),c)]
        blop [] = []
        blop ((_,[]):abcs) = blop abcs
        blop ((a,(b,c):bcs):abcs) = ((a,b),c) : blop ((a,bcs):abcs)
    --}
    
data AssocList k v = AssocList [MapEntry k v] deriving Show

instance ReadMap (AssocList) where
    get _ (AssocList []) = Nothing
    get k (AssocList ((k',v):es)) 
        | k'==k = Just v
        | otherwise = get k (AssocList es)

    entries (AssocList es) = es    
    
instance Map (AssocList) where
    
    put k v (AssocList es) = AssocList ((k,v):es)

    remove k (AssocList es) = AssocList $ (filter (\(k',_)->k/=k')) es

    type (EmptyMapContext AssocList k v) = Void
    
    emptyMap _ = AssocList []




data LayeredMap b t k v = LayeredMap (b k v) (t k (Maybe v)) 

instance (Show (b k v), Show (t k (Maybe v))) => Show (LayeredMap b t k v) where
    show (LayeredMap b t) = "LayeredMap " ++ (show b) ++ " " ++ (show t)

instance (ReadMap b, Map t) => ReadMap (LayeredMap b t) where
    get k (LayeredMap bottom top) = pick (get k bottom) (get k top) where
        pick _ (Just v) = v
        pick v Nothing = v

    entries (LayeredMap bottom top) = minus (entries bottom) top ++ just (entries top) where
        minus bs t = filter (\(k,v) -> not $ containsKey k t) bs
        just :: [(k, Maybe v)] -> [(k, v)]
        just = foldr grab []
        grab :: (k, Maybe v) -> [(k,v)] -> [(k,v)]
        grab (_,Nothing) = id
        grab (k, Just v) = ((k,v):)
        

instance (ReadMap b , Map t ) => Map (LayeredMap b t) where
    put k v (LayeredMap bottom top) = LayeredMap bottom (put k (Just v) top)

    remove k (LayeredMap bottom top) = LayeredMap bottom (put k Nothing top)

    type (EmptyMapContext (LayeredMap b t) k v) = (b k v, EmptyMapContext t k (Maybe v))

    emptyMap (b, tc) = LayeredMap b ((putEntries (map (\k -> (k,Nothing)) (keys b))) (emptyMap tc))


data TransformMap m k v k' v' = TransformMap (k->k') (v->v') (k'->k) (v'->v) (m k v)

instance (Show (m k v)) => Show (TransformMap m k v k' v') where
    show (TransformMap _ _ _ _ m) = "TransformMap {"++ show m ++ "}"

instance (ReadMap m, Eq k) => ReadMap (TransformMap m k v) where
    get k (TransformMap kf vf kb vb m) = fmap vf (get (kb k) m)
    entries (TransformMap kf vf kb vb m) = map (\(k,v)->(kf k, vf v)) (entries m)

instance (Map m, Eq k) => Map (TransformMap m k v) where
    put k v (TransformMap kf vf kb vb m) = TransformMap kf vf kb vb (put (kb k) (vb v) m)
    remove k (TransformMap kf vf kb vb m) = TransformMap kf vf kb vb (remove (kb k) m)
    type EmptyMapContext (TransformMap m k v) k' v' = (k->k', v->v', k'->k, v'->v, EmptyMapContext m k v)
    emptyMap (kf, vf, kb, vb, c) = TransformMap kf vf kb vb (emptyMap c)  


blonk = emptyMap undefined :: AssocList Double Double
glonk = emptyMap ((*2),(*3),(/2),(/3),undefined) :: TransformMap AssocList Double Double Double Double
main = do
    print "start"
    print $ put 6 6 glonk
    print "end"
