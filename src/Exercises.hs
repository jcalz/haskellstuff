module Exercises where

import qualified System.Random as Random

import Debug.Trace

myLast :: [a] -> a
--myLast = last
--myLast = \x -> x !! (length x - 1)
myLast = foldr1 (flip const)

(!!-) :: [a] -> Int -> a
as !!- i = case 
        foldr (\a acc -> case acc of 
            (Left 0) -> Right a
            (Left n) -> Left (n-1)
            ra -> ra) (Left i) as
    of 
        (Right a) -> a
        (Left a) -> error ("Need at least "++show (a+1)++" more elements")


myButLast :: [a] -> a
myButLast = (!!- 1)
--myButLast [a,_] = a
--myButLast (a:as) = myButLast as

getElementAt :: [a] -> Int -> a
getElementAt as = (!!) as . (subtract 1)

myLength :: [a] -> Int
myLength = foldr (const succ) 0

myReverse :: [a] -> [a]
--myReverse as = fst $ foldr (\a (ls,r:rs) -> (r:ls, rs)) ([],as)
myReverse as = myReverse2 (as,[]) where
    myReverse2 :: ([a],[a]) -> [a]
    myReverse2 ([],x) = x
    myReverse2 (a:y,x) = myReverse2 (y, a:x)

myUntil :: (x -> Bool) -> (x -> x) -> x -> x
myUntil pred f x = if (pred x) then x else myUntil pred f (f x)


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = (x == myReverse x)

data NestedList a = Elem a | NestedList [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (NestedList ls) = concat (map myFlatten ls)

compress :: (Eq a) => [a] -> [a]
compress = foldr compr [] where
    compr :: (Eq a) => a -> [a] -> [a]
    compr a [] = [a]
    compr a bs@(b:_) = if (a == b) then bs else (a:bs)  

pack :: (Eq a) => [a] -> [[a]]
pack = foldr p [] where
    p :: (Eq a) => a -> [[a]] -> [[a]]
    p a [] = [[a]]
    p a bbbs@(bs@(b:_):bbs) = if (a == b) then ((a:bs):bbs) else ([a]:bbbs)

encode :: (Eq a) => [a] -> [(Int,a)]
encode = map (\as -> (length as, head as)) . pack

data Quantity a = Single a | Multiple Int a deriving Show
modifiedEncode :: (Eq a) => [a] -> [Quantity a]
modifiedEncode = map blah . encode where
   blah (1,a) = Single a
   blah (n,a) = Multiple n a 

decodeModified :: [Quantity a] -> [a]
decodeModified = concat . (map blah) where
    blah (Single a) = [a]
    blah (Multiple n a) = replicate n a

encodeDirect :: (Eq a) => [a] -> [Quantity a]
encodeDirect = foldr f [] where
    f a [] = [Single a]
    f a (Single b : cs) = if (a==b) then (Multiple 2 b : cs) else (Single a : Single b : cs)
    f a (Multiple n b : cs) = if (a==b) then (Multiple (n+1) b : cs) else (Single a : Multiple n b : cs)

dupli :: [a] -> [a]
--dupli = foldr (\a aa -> a:a:aa) []
dupli = concatMap (\a -> [a,a])

repli :: [a] -> Int -> [a]
repli = flip (\i -> (concatMap (\a -> replicate i a)))

dropEvery :: [a] -> Int -> [a]
dropEvery as i = dropInner as (i-1) where
    dropInner [] _ = []
    dropInner (a:as) 0 = dropInner as (i-1)
    dropInner (a:as) n = a : (dropInner as (n-1))

dropEvery' :: [a] -> Int -> [a]
dropEvery' as i = map fst $ filter ((/= i) . snd) (zip as (cycle [1..i]))

split :: [a] -> Int -> ([a],[a])
split as 0 = ([],as)
split [] _ = ([],[])
split (a:as) i = (\(l,r) -> (a:l,r)) (split as (i-1))

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice _ 1 0 = []
slice (a:as) 1 k = a : (slice as 1 (k-1))
slice (a:as) j k = slice as (j-1) (k-1) 

rotate :: [a] -> Int -> [a]
rotate as i = (\(l,r) -> r ++ l ) (split as (i `mod` length as))

removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt as 0 = as
removeAt (a:as) 1 = as
removeAt (a:as) n = a: (removeAt as (n-1))

removeAtAndKeep :: [a] -> Int -> (a, [a])
removeAtAndKeep as i = foldr (\(a,k) (e,as) -> if (i==k) then (a, as) else (e, a:as)) (undefined,[]) (zip as [1..])


insertAt :: a -> [a] -> Int -> [a]
insertAt a as i = l ++ (a:r) where (l,r) = split as (i-1)

range :: Int -> Int -> [Int]
--range i j = [i..j]
range i j = if (i > j) then [] else (i : range (i+1) j)

rndSelect :: (Random.RandomGen g) => g -> [a] -> Int -> ([a], g)
rndSelect g as i = rndSelectPrefix as i ([],g) where
    rndSelectPrefix :: (Random.RandomGen g) => [a] -> Int -> ([a],g) -> ([a],g)
    rndSelectPrefix _ 0 x = x    
    rndSelectPrefix as i (ret, g) = rndSelectPrefix as' (i-1) (a : ret, g') where
        (k, g') = Random.randomR (1, length as) g
        (a, as') = removeAtAndKeep as k
    
diffSelect :: (Random.RandomGen g) => g -> Int -> Int -> ([Int], g)
diffSelect g i n = rndSelect g [1..n] i

rndPermu :: (Random.RandomGen g) => g -> [a] -> ([a],g)
rndPermu g as = rndSelect g as (length as)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (a:as) = (map (a:) (combinations (n-1) as)) ++ combinations n as

combinationsWithComplement :: Int -> [a] -> [([a],[a])]
combinationsWithComplement 0 as = [([],as)]
combinationsWithComplement _ [] = []
combinationsWithComplement n (a:as) = 
    (map (\(y,n) -> (a:y,n)) (combinationsWithComplement (n-1) as)) ++ 
    (map (\(y,n) -> (y,a:n)) (combinationsWithComplement n as))


group :: [a] -> [Int] -> [[[a]]] 
group as ns = map (reverse . tail) $ foldr groupingMore [[as]] (reverse ns) where
    groupingMore n aaas = concat (map (grouping n) aaas)    
    grouping n (as:aas) = map (\(y,n) -> n:y:aas) (combinationsWithComplement n as)

sortBy' :: (Show  a) => (a -> a -> Ordering) -> [a] -> [a]
-- merge sort?
sortBy' _ [] = []
sortBy' _ [a] = [a]
sortBy' cmp as = merge (sortBy' cmp left) (sortBy' cmp right) where
    (left, right) = split as (length as `div` 2)
    merge ls [] = ls
    merge [] rs = rs
    merge lls@(l:ls) rrs@(r:rs) = if (cmp l r /= GT) then (l : (merge ls rrs)) else (r : (merge lls rs))

compare' :: (Ord a) => a -> a -> Ordering
compare' l r 
    | l < r = LT
    | l ==r = EQ
    | otherwise = GT

compareOn :: (Ord a) => (b -> a) -> b -> b -> Ordering
compareOn f l r
    | fl < fr = LT
    | fl == fr = EQ
    | otherwise = GT
    where fl = f l
          fr = f r

-- merge Sort
sort' :: (Ord a) => [a] -> [a]
sort' [] = []
sort' [a] = [a]
sort' as = merge (sort' left) (sort' right) where
    (left, right) = split as (length as `div` 2)
    merge ls [] = ls
    merge [] rs = rs
    merge lls@(l:ls) rrs@(r:rs) = if (l <= r) then (l : (merge ls rrs)) else (r : (merge lls rs))


data SortOn a b = SortOn {sortOnValue :: a, sortOnKey :: b}

instance (Eq b) => Eq (SortOn a b) where
    (SortOn _ x) == (SortOn _ y) = x == y

instance (Ord b) => Ord (SortOn a b) where
    (SortOn _ x) `compare` (SortOn _ y) = x `compare` y

sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f = map (sortOnValue) . sort' . map (\a -> (SortOn a (f a)))


data ReverseOrder a = ReverseOrder a

instance (Eq a) => Eq (ReverseOrder a) where
    (ReverseOrder x) == (ReverseOrder y) = x == y
instance (Ord a) => Ord (ReverseOrder a) where
    compare (ReverseOrder x) (ReverseOrder y) = compare y x

tally :: (Eq a) => [a] -> [(Int, a)]
tally = foldl addToTally [] where
    addToTally :: (Eq a) =>  [(Int, a)] -> a -> [(Int, a)]
    addToTally [] a = [(1,a)]
    addToTally (t@(cnt,b):ts) a = if (a==b) then (cnt+1,b):ts else t:(addToTally ts a)


lsort :: [[a]] -> [[a]]
lsort = sortOn length

firstIndexOf :: (Eq a) => [a] -> a -> Maybe Int
firstIndexOf [] _ = Nothing
firstIndexOf (b:bs) a = if (a==b) then (Just 0) else fmap (+1) (firstIndexOf bs a)

lfsort :: [[a]] -> [[a]]
lfsort as = sortOn (firstIndexOf (map snd (sortOn fst (tally (map length as)))) . length) as 

isPrime :: Int -> Bool
isPrime n = n>1 && all (\f -> (n `mod` f) /= 0) [2..(floor (sqrt (fromIntegral n)))] -- slow?

gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' (a `mod` b) a

coprime :: Int -> Int -> Bool
coprime = curry ((==1) . uncurry gcd)

totient :: Int -> Int
totient a = length $ filter (==True) $ map (coprime a) [1..(a-1)]

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (a:as) = if (p a) then Just a else find p as

findSmallestFactorAtLeast :: Int -> Int -> Maybe Int
findSmallestFactorAtLeast m n = find (\a -> (n `mod` a) == 0) [m..(floor (sqrt (fromIntegral n)))]

findSmallestFactor :: Int -> Maybe Int
findSmallestFactor = findSmallestFactorAtLeast 2

isPrime' :: Int -> Bool
isPrime' a = (a>1) && findSmallestFactor a == Nothing

factor :: Int -> [Int]
factor n = case (findSmallestFactor n) of
    Nothing -> [n]
    (Just k) -> k : (factor (n `div` k))

factorMult :: Int -> [(Int,Int)]
factorMult = map (\(x,y)->(y,x)) . tally . factor

totient' :: Int -> Int
totient' = (foldr (\(p,m) -> ((p ^ (m-1) * (p-1)) *)) 1) . factorMult 

primesR :: Int -> Int -> [Int]
primesR a b = filter isPrime [a..b]

goldbach :: Int -> (Int, Int)
goldbach a = (a-x, x) where 
    Just x = find (isPrime) $ map (a -) $ primesR 2 (a `div` 2)

goldbachList :: Int -> Int -> [(Int, Int, Int)]
goldbachList a b = map (\x -> (x, fst (goldbach x), snd (goldbach x))) [ x | x <- [a..b], x > 2, x `mod` 2 == 0 ]

goldbachListGreater :: Int -> Int -> [(Int, Int, Int)]
goldbachListGreater b p = filter (\(_,a,_) -> a > p) $ goldbachList 2 b



main = do
    print "start"
    print $ goldbachListGreater 3000 50
    print "end"