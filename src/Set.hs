{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Set where

class (Set s) where
    contains :: (Eq v) => v -> s v -> Bool
    add :: (Eq v) => v -> s v -> s v
    remove :: (Eq v) => v -> s v -> s v
    emptySet :: s v

    toList :: (Eq v) => s v -> [v]

    addAll :: (Eq v) => [v] -> s v -> s v
    addAll = flip (foldr add)

    removeAll :: (Eq v) => [v] -> s v -> s v
    removeAll = flip (foldr remove)

    fromList :: (Eq v) => [v] -> s v
    fromList = (flip addAll) emptySet    

    union :: (Set t, Eq v) => t v -> s v -> s v
    union = addAll . toList

    intersection :: (Set t, Eq v) => t v -> s v -> s v
    intersection a b = fromList (filter (`elem` (toList a)) (toList b))

    cartesianProduct :: (Set t, Eq v, Eq w) => t v -> s w -> s (v,w)
    cartesianProduct a b = fromList [(v,w) | v <- toList a, w <- toList b]

    difference :: (Set t, Eq v) => s v -> t v -> s v
    difference= flip (removeAll . toList)
    
data AnySet v = forall s. (Set s) => AnySet (s v)

instance Set ([]) where
    contains = elem
    add = (:)
    remove v = filter (/= v) 
    emptySet = []
    toList = id
    
instance Set (AnySet) where
    contains v (AnySet s) = contains v s
    add v (AnySet s) = AnySet (add v s)
    remove v (AnySet s) = AnySet (remove v s)
    emptySet = AnySet []
    toList (AnySet s) = toList s

data LayeredSet sb sa sr v = LayeredSet {baseSet :: sb v, addSet :: sa v, removeSet :: sr v}

instance (Set sb, Set sa, Set sr) => Set (LayeredSet sb sa sr) where
    contains v (LayeredSet b a r) = (not (contains v r)) && (contains v b || contains v a)
    add v (LayeredSet b a r) = LayeredSet b (add v a) (remove v r)
    remove v (LayeredSet b a r) = LayeredSet b (remove v a) (add v r)
    emptySet = undefined
    toList (LayeredSet b a r) = toList (difference (union b a) r)
    