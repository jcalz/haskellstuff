{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module DataSet where

import Data.List (nub)
import Debug.Trace

data SetFactory v = SetFactory {emptySet :: Set v, fromList :: [v] -> Set v, fromSet :: Set v -> Set v, showFactory :: (Show v) => String}

data Set v = Set {
    contains :: v -> Bool,
    containsAll :: [v] -> Bool,
    add :: v -> Set v,
    remove :: v -> Set v,
    size :: Integer,
    toList :: [v],
    addAll :: [v] -> Set v,
    keepOnly :: (v -> Bool) -> Set v,
    removeAll :: [v] -> Set v,
    union :: Set v -> Set v,
    intersection :: Set v -> Set v,
    difference :: Set v -> Set v,
    cartesianProduct :: forall w. (Eq w) => Set w -> Set (v,w),
    setFactory :: forall w. (Eq w) => SetFactory w,
    showSet :: (Eq v, Show v) => String
    }

instance (Eq v) => (Eq (Set v)) where
    s == t = (size (difference s t) == 0) && (size (difference t s) == 0)
    
instance (Eq v, Show v) => Show (Set v) where
    show = showSet

instance (Eq v, Show v) => Show (SetFactory v) where
    show = showFactory
    
    
length' :: [a] -> Integer
length' [] = 0
length' (a:as) = seq (length' as) (1+(length' as))

nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (a:as) = a : filter (/= a) (nub' as)

listSet :: (Eq v) => [v] -> Set v
listSet vs' = let 
    vs = nub vs' 
    this = Set {
  size = length' vs,
  contains = (`elem` vs),
  containsAll = defContainsAll this,
  add = \v -> listSet (v:vs),
  remove = defRemove this,
  toList = vs,
  addAll = \ws -> listSet $ ws ++ vs,  
  keepOnly = \pred -> listSet $ filter pred vs,
  removeAll = defRemoveAll this,
  union = defUnion this,
  intersection = defIntersection this,
  difference = defDifference this,
  cartesianProduct = defCartesianProduct this,
  setFactory = listSetFactory,
  showSet = "ListSet " ++ (show vs)
} in this

listSetFactory :: (Eq v) => SetFactory v
listSetFactory = SetFactory {
    emptySet = listSet [],
    fromList = listSet,
    fromSet = listSet . toList,
    showFactory = "ListSetFactory"
}



layeredSetFactory :: (Eq v) => Set v -> SetFactory v -> SetFactory v -> SetFactory v
layeredSetFactory base removeFactory addFactory = let 
    this = SetFactory {
        emptySet = layeredSet base (removeFactory ^. fromSet $ base) (addFactory ^. emptySet),
        fromList = \vs -> addAll (this ^. emptySet) vs,
        fromSet = \s -> layeredSet base (removeFactory ^. fromSet $ difference base s) (addFactory ^. fromSet $ difference s base),
        showFactory = "LayeredSetFactory " ++ (show base) 
    } 
    in this

layeredSet :: (Eq v) => Set v -> Set v -> Set v -> Set v
layeredSet base minus' plus' = let 
    plus = difference (difference plus' base) minus'
    minus = intersection minus' base
    this = Set {
    contains = \v -> (not (minus ^. contains $ v)) && ((plus ^. contains $ v) || (base ^. contains $ v)),
    add = \v -> layeredSet base  (minus ^. remove $ v) (plus ^. add $ v),
    remove = \v -> layeredSet base (minus ^. add $ v) (plus ^. remove $ v), 
    setFactory = layeredSetFactory ((base ^. setFactory) ^. emptySet) (minus ^. setFactory) (plus ^. setFactory), -- lame, setFactory cannot provide "the same" set
    containsAll = defContainsAll this,
    size = defSize this,
    toList = filter (\v -> not (minus ^. contains $ v)) (nub (toList base ++ toList plus)),
    addAll = defAddAll this,
    keepOnly = defKeepOnly this,
    removeAll = foldr (flip remove) this,
    union = defUnion this,
    intersection = defIntersection this,
    difference = defDifference this,
    cartesianProduct = defCartesianProduct this,
    showSet = "LayeredSet " ++ (show base) ++ " - " ++ (show minus) ++ " + " ++ (show plus)
} in this


defSize :: (Eq v) => Set v -> Integer
defSize s = length' $ nub $ toList s

defContains :: (Eq v) => Set v -> v -> Bool
defContains s v = v `elem` (toList s)

defAdd :: (Eq v) => Set v -> v -> Set v
defAdd s v = (fromList (setFactory s)) (v: (toList s))

defRemove :: (Eq v) => Set v -> v -> Set v
defRemove s v = keepOnly s (/= v)

defContainsAll :: (Eq v) => Set v -> [v] -> Bool
defContainsAll s vs = all (contains s) vs

defAddAll :: (Eq v) => Set v -> [v] -> Set v
defAddAll s vs = foldr (flip add) s vs

defKeepOnly :: (Eq v) => Set v -> (v -> Bool) -> Set v
defKeepOnly s pred = (fromList (setFactory s)) (filter pred (toList s))

defRemoveAll :: (Eq v) => Set v -> [v] -> Set v
defRemoveAll s vs = keepOnly s (not . (`elem` vs))

defUnion :: (Eq v) => Set v -> Set v -> Set v
defUnion s t = (addAll s) (toList t)

defIntersection :: (Eq v) => Set v -> Set v -> Set v
defIntersection s t = keepOnly s (contains t)

defDifference :: (Eq v) => Set v -> Set v -> Set v
defDifference s t = keepOnly s (not . contains t)

defCartesianProduct :: (Eq v, Eq w) => Set v -> Set w -> Set (v,w)
defCartesianProduct s t = (fromList (setFactory s)) [(v,w) | v <- (toList s), w <- (toList t)]

setMap :: (Eq v, Eq w) => (v -> w) -> Set v -> Set w
setMap f s = ((setFactory s) ^. fromList) (map f (toList s))

----

foo = fromList listSetFactory [8,5,2,7,5,9,2]
goo = fromList listSetFactory [4,2,1,7,0,9,2]

setId (s,t) = (union (difference s t) (intersection s t) , union (difference t s) (intersection s t))


a ^. b = b a



--fac = layeredSetFactory (listSet [1,2,3]) (listSetFactory) (listSetFactory)

hoo = layeredSet (listSet [1,2,3]) (listSet []) (listSet [])

main = do
    print "begin"
    --print $ ( foo ^. addAll $ [14, 19, 20, 2, 5, 9, 11] ) ^. add $ 72
    --print $ foo ^. removeAll $ [1, 2, 3]
    --print $ (foo,goo) == setId (foo,goo)
    --print $ take 100 $ nub' $ map (`div` 4) [0..]
    print $ (hoo ^. addAll $ [4,5,6])  ^. removeAll $ [2,4,6]
    print "end"
    