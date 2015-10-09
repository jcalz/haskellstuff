module Sieve where

import Data.Lists

-- mergeAll takes a sorted list of sorted lists and produces a single sorted list
mergeAll :: (Ord a) => [[a]] -> [a]
mergeAll [] = []
mergeAll ([]:ls) = mergeAll ls
mergeAll ((a:as):ls) = a : (mergeAll $ insert as ls)

-- here's another way to write it
mergeAll' :: (Ord a) => [[a]] -> [a]
mergeAll' = foldr imerge [] where
    imerge [] bs = bs
    imerge (a:as) bs = a : merge as bs

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
primes = 2 : diffSorted [3..] (mergeAll (map sieveMultiples primes))

main = do
    print "Sieve start"
    print $ take 100 primes
    print "Sieve end"