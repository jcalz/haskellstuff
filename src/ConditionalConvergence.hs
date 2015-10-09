module ConditionalConvergence where

import Data.Ratio
import Debug.Trace

splitStream :: (Num a, Ord a) => [a] -> ([a],[a])
splitStream (a:as) = if (a<0) then (ps, a:ns) else (a:ps, ns) where (ps, ns) = splitStream as

data StreamInterleave a = StreamInterleave {_positives :: [a], _negatives ::[a] , _runningTotal :: a, _curVal :: a} deriving Show

iterStream :: (Num a, Ord a) => a -> StreamInterleave a -> StreamInterleave a
iterStream tot (StreamInterleave ps@(p:pps) ns@(n:nns) curTot _) = StreamInterleave ps' ns' (curTot+v) v where
    (ps', ns', v) = if (curTot<=tot) then (pps, ns, p) else (ps, nns, n)

blop :: (Num a, Ord a) => a -> [a] -> [a]
blop tot as = map (\(StreamInterleave _ _ _ v) -> v) $ iterate (iterStream tot) (StreamInterleave ps ns 0 0) where
    (ps,ns)=(splitStream as)

runningTotal :: (Num a) => [a] -> [a]
runningTotal a = r where
    r = zipWith (+) a (0:r)

--seq =

--mainSeq = (((iterate (2 *) (1)) >>= (\i -> take (fromIntegral i) (repeat (1 % i)))) >>= (\r -> [r, -r])) 

bouncy [] = []
bouncy [a] = [a]
bouncy (a:b:as) = (a:(-b):bouncy as)

mainSeq = bouncy $ map (1 %) [1..]

getAt = flip (!!) 

main = do
    print "start"
    --print $ take 20 $ mainSeq
    --print $ take 20 $ snd $ splitStream mainSeq
    --print $ take 100 $ (map fromRational) $ blop (1%3) mainSeq
    print $ getAt 100000 $ runningTotal $ blop (0.5) $ (map fromRational) mainSeq
--    print $ take 20 $ (map fromRational) $ runningTotal mainSeq
    print "end"