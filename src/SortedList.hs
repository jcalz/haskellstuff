module SortedList where

import Data.Monoid

data SortedList a = SNil | SCons a (SortedList a)

cons :: (Ord a) => a -> SortedList a -> SortedList a
cons = SCons

uncons :: (Ord a) => SortedList a -> Maybe (a, SortedList a)
uncons SNil = Nothing
uncons (SCons a as) = Just (a, as) where
    a' = case as of
        SNil -> a
        SCons b _ -> if (a <= b) then a else b


instance (Show a) => Show (SortedList a) where
    show SNil = "snil"
    show (SCons a as) = show a ++ " <: " ++ show as

instance (Eq a) => Eq (SortedList a) where
    SNil == SNil = True
    SCons a as == SCons b bs = (a==b) && (as==bs)
    _ == _ = False    

instance (Ord a) => Ord (SortedList a) where
    compare SNil SNil = EQ
    compare SNil _ = LT
    compare _ SNil = GT
    compare (SCons a as) (SCons b bs) 
        | a<b = LT
        | a>b = GT
        | otherwise = compare as bs
                    
    


-- smart constructor and reader

snil :: (Ord a) => SortedList a
snil = SNil


(<:) :: (Ord a) => a -> SortedList a -> SortedList a
(<:) = SCons
infixr 5 <:


sHead :: (Ord a) => SortedList a -> a
sHead as = a where 
    Just (a,_) = uncons as

sTail :: (Ord a) => SortedList a -> SortedList a
sTail as = as' where
    Just (_,as') = uncons as

nats :: SortedList Int
nats = natsStartingWith 0 where
    natsStartingWith n = n <: natsStartingWith (n+1)

merge :: (Ord a) => SortedList a -> SortedList a -> SortedList a
merge as bs = case (uncons as, uncons bs) of
    (Nothing, _) -> bs
    (_, Nothing) -> as
    (Just (a,as'), Just (b, bs')) ->
        if a<=b then (a <: merge as' bs) else (b <: merge as bs')

insert :: (Ord a) => SortedList a -> SortedList (SortedList a) -> SortedList (SortedList a)
insert as ass = case (uncons ass) of
    Nothing -> as <: snil
    Just (as', ass') -> if (as <= as') then (as <: ass) else (as' <: (insert as ass'))
    

union :: (Ord) a => SortedList (SortedList a) -> SortedList a
union ass =  case (uncons ass) of
    Nothing -> snil
    Just (as,ass') -> case (uncons as) of
        Nothing -> union ass'
        Just (a, as') -> a <: union (insert as' ass')

nub :: (Ord a) => SortedList a -> SortedList a
nub as = case (uncons as) of
    Nothing -> snil
    Just (a, as') -> case (uncons as') of
        Nothing -> a <: snil
        Just (a', _) -> if (a==a') then (nub as') else (a <: nub as')
    

badSortedList :: SortedList Int
badSortedList = bl 10 where
    bl n = n <: bl (n-1)

sTake :: (Ord a) => Int -> SortedList a -> SortedList a
sTake 0 _ = snil
sTake i as = case (uncons as) of
    Nothing -> snil
    Just (a, as') -> a <: (sTake (i-1) as')


-- warning: f must be monotonically increasing!!!
sMap :: (Ord a, Ord b) => (a -> b) -> SortedList a -> SortedList b
sMap f as = case (uncons as) of
    Nothing -> snil
    Just (a,as') -> (f a) <: sMap f as'
    
sIterate :: (Ord a) => (a -> a) -> a -> SortedList a
sIterate f a = if (a > f a) then 
        error "sIterate creating out of order SortedList is bad" else
        a <: sIterate f (f a)

integersGreaterThanTwo = sIterate (+1) 3

prim :: SortedList Int    
--prim = 2 <: 3 <: 5 <: 7 <: 11 <: 13 <: snil
prim = 2 <: (integersGreaterThanTwo `minus` union sieveMults)

sieveMults = sMap f prim where
    f :: Int -> SortedList Int
    f a =its (a * a) where
        its b = b <: its (a + b)

minus :: (Ord a) => SortedList a -> SortedList a -> SortedList a
minus as bs = case (uncons as, uncons bs) of
    (Nothing, _) -> snil
    (_, Nothing) -> as
    (Just (a, as'), Just (b,bs')) -> case (compare a b) of
        LT -> a <: minus as' bs
        EQ -> minus as' bs
        GT -> minus as bs'

main = do
    print "SortedList start"
    print $ sTake 10 $ prim
    print "SortedList end"