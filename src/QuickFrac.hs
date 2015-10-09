module QuickFrac where
import Data.Ratio
import Debug.Trace
import Data.List

--findNaboveMThatSumToF 

t x = trace (show x) x

finds :: Integer -> Integer -> Rational -> [[Integer]]
--finds n m f  | trace ("finds "++show n ++" "++show m++" ("++ show f ++ ")") False = error "hey" 
finds n m f  -- find all sets of n natural numbers, all of which are at least m, such that the sum of their reciprocals equals f    
    | n==0 && f==0 = [[]] -- the empty set is the unique set of zero numbers which sum to zero 
    | f <= 0 = [] -- if n>0 and f<=0, or if n==0 and f/=0 then there are no solutions 
    | n%m < f = [] -- if n/m is less than f then there are no solutions, since n/m is the maximum possible value for the sum of the recips
    | otherwise = -- the recursive solution contains
        finds n (m+1) f -- any solution that doesn't contain m (that is, all numbers are at least m+1),
        ++ 
        map (m:) (finds (n-1) m (f-1%m)) -- m appended to any set of n-1 numbers at least m whose recips add up to f-1/m 

-- n above m that sum to f
-- at some M>m, 1/(M+1) + 1/(M+2) + ... + 1/(M+n) < f and then it's impossible for the first digit to be less than M  

fives = finds 5 1 (1)

norep :: (Eq a) => [[a]] -> [[a]]
norep = filter (\x -> x == nub x)

noreps = filter (\x -> x == nub x) fives

main = do
    print "QuickFrac"
    --print  $ sort fives
    --print $ length fives
    --print $ sort noreps
    --print $ length noreps
    --print $ finds 3 12 (22%7 - (83711 % 27720))
    print $ foldr (+) 0 (map (1%) ([1..11]++[15,18,1320]))
    print "end"