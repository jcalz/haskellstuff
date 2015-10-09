module SieveOld where

import qualified Data.Set as Set
import Data.List
import Data.Lists
import Debug.Trace


data SieveIterator = SI {siCur :: Integer, siPrime :: Integer} deriving (Eq, Ord, Show) 

-- SI cur prime 



minCur :: Set.Set SieveIterator -> Integer
minCur s = if (Set.null s) then 0 else (siCur $ Set.findMin s)


step :: Integer -> Set.Set SieveIterator -> (Bool, Set.Set SieveIterator)
step cur iters = (isPrime, iters') where
    isPrime = (minCur iters /= cur)
    iters' = if (isPrime) then
        Set.insert (SI (cur*cur) cur) iters else
        Set.union rest curs' where
            (curs, rest) = Set.split (SI cur cur) iters
            curs' = Set.map (\(SI c p) -> (SI (c+p) p)) curs

stepsame :: (Integer, Bool, Set.Set SieveIterator) -> (Integer, Bool, Set.Set SieveIterator)
stepsame (cur, _, iters) = (cur+1, p, iters') where (p, iters') = step (cur+1) iters


primeys = iterate stepsame (1, False, Set.empty)

primes = map (\(a,_,_)->a) $ filter (\(_,b,_)->b) primeys                                                    

---

-- insertBy' puts an element into a sorted list to produce a new sorted list
insertBy' :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy' _ a [] = [a]
insertBy' compare a as@(a':as') = if (a `compare` a' /= GT) then (a:as) else (a':(insertBy' compare a as'))



-- mergeAll takes a sorted list of sorted lists and produces a single sorted list
mergeAll :: (Ord a) => [[a]] -> [a]
mergeAll [] = []
mergeAll ([]:ls) = mergeAll ls
--mergeAll ((a:as):ls) = a : (mergeAll $ insertBy' (\(a:_) (b:_) -> compare a b) as ls)  
mergeAll ((a:as):ls) = a : (mergeAll $ insert as ls)

mergeAll' :: (Ord a) => [[a]] -> [a]
mergeAll' [] = []
mergeAll' (l:ls) = merge l (mergeAll' ls)

--diffSorted produces the difference of two sorted lists and produces a third sorted list 
diffSorted :: (Ord a) => [a] -> [a] -> [a]
diffSorted [] _ = []
diffSorted as [] = as
diffSorted as@(a:as') bs@(b:bs') = case (a `compare` b) of
    LT -> a : diffSorted as' bs 
    EQ -> diffSorted as' bs
    GT -> diffSorted as bs'

-- sieveMultiples takes a prime number p and returns the (sorted) list of all values to cross off in the sieve,
 -- this is all multples of p, starting with p squared.
sieveMultiples :: Integer -> [Integer]
sieveMultiples p = iterate (+p) (p*p)

-- finally, the sieve: 2 is prime, followed by all the numbers greater than 2 which are not crossed off by any
--   prime in the sieve.
primes' = 2 : diffSorted [3..] (mergeAll (map sieveMultiples primes'))

main = do
    print "Sieve start"
    print $ take 100 primes'
    print $ mergeAll [[0,1000],[1,2,3,7,8,9],[4,5,6,9]]
    print "Sieve end"
