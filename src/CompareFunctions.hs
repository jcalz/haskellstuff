module CompareFunctions where

import Data.Word

bigVal :: Int
bigVal = 2 ^ 20

instance (Bounded a, Enum a, Eq b) => Eq (a -> b) where
    f == g = all (\a -> (f a == g a)) (take bigVal [minBound..maxBound])

instance (Bounded a, Enum a, Ord b) => Ord (a -> b) where
    compare f g = compareVals f g (take bigVal [minBound..maxBound]) where
        compareVals :: (Ord b) => (a -> b) -> (a -> b) -> [a] -> Ordering
        compareVals f g [] = EQ
        compareVals f g (a:as) = 
            let headcomp = compare (f a) (g a) in
                if (headcomp /= EQ) then headcomp else compareVals f g as
                
                
                                     
                                    



main = do
    print "start"
    print $ (+ (2 :: Word64)) == (2 +)
    print "end"