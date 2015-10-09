{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Set where

import qualified Map as M
import qualified Isomorphism as I
import Data.List

--type family STC s :: (Set s) => * -> *

class Set s where
    type V s
    type ST s :: * -> *
        
    emptySet :: s
    add :: (V s) -> s -> s
    contains :: (V s) -> s -> Bool
    remove :: (V s) -> s -> s

    values :: s -> [V s]

    addAll :: [V s] -> s -> s
    addAll = flip (foldr add)

    removeAll :: [V s] -> s -> s
    removeAll = flip (foldr remove)

    fromList :: [V s] -> s
    fromList = (flip addAll) emptySet

    (∩) :: (Set s, Set t, V s ~ V t) => s -> t -> s
    s ∩ t = fromList (filter (flip contains s) (values t))

    (∪) :: (Set s, Set t, V s ~ V t) => s -> t -> s
    s ∪ t = addAll (values t) s
    
    (-) :: (Set s, Set t, V s ~ V t) => s -> t -> s
    s - t = removeAll (values t) s

    (×) :: (Set s, Set t, Set (ST s (V s, V t)), (V (ST s (V s, V t)) ~ (V s, V t))) => s -> t -> (ST s (V s, V t))
    s × t = fromList [(se,te) | se <- values s, te <- values t ]

    

givenNames = ["Alice", "Betty", "Carol"]
surnames = ["Dooley", "Easton", "Finch"]
names = (givenNames × surnames)

main = do
    print "start"
    print names
    print "end"

intersection :: (Set s, Set t, V s ~ V t) => s -> t -> s
intersection = (∩)

union :: (Set s, Set t, V s ~ V t) => s -> t -> s
union = (∪)

difference :: (Set s, Set t, V s ~ V t) => s -> t -> s
difference = (Set.-)

(...) = (.) . (.)

instance (Eq v) => Set [v] where
    type V [v] = v
    type ST [v] = []
    emptySet = []
    add = nub ... (:)
    contains = elem
    values = nub
    remove v = filter (/= v)

data MapSet m = MapSet {unMapSet::m, defaultValue::M.V m}

instance (M.Map m) => Set (MapSet m) where
    type V (MapSet m) = M.K m
    type ST (MapSet m) = []
    emptySet = MapSet M.emptyMap undefined -- don't know what value to put
    add v (MapSet m d) = MapSet (M.put v d m) d 
    contains v (MapSet m _) = M.containsKey v m
    remove v (MapSet m d) = MapSet (M.remove v m) d
    values (MapSet m _) = M.keys m

 