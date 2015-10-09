module Stream (Stream, readStream, makeStreamFromList, main) where


data Stream a = S {readStream :: (Maybe a, Stream a)}

makeStreamFromList :: [a] -> Stream a
makeStreamFromList [] = let this = S (Nothing, this) in this
makeStreamFromList (a:as) = S (Just a, makeStreamFromList as)

consumeStream :: (Stream a -> (Maybe b, Stream a)) -> Stream a -> ([b], Stream a)
consumeStream f s = case (f s) of
    (Nothing, s') -> ([], s')
    (Just b, s') -> (b : bs, s'') where
        (bs, s'') = consumeStream f s'

sumUntilGreatherThan :: Int -> Stream Int -> [Int]
sumUntilGreatherThan maxSum = fst . consumeStream (f 0) where
    f curSum s = 
        if (curSum >= maxSum) 
        then (Just curSum, s) 
        else case (readStream s) of
            (Just i, s') -> f (curSum + i) s'
            (Nothing, s') -> (Nothing, s')

makeListFromStream :: Stream a -> [a]
makeListFromStream = fst . (consumeStream readStream)

ss = makeStreamFromList (map (floor . log) [1..]) 

main = do
    print "start STREAM"  
    print $ take 100 (makeListFromStream ss)
    print $ take 100 (sumUntilGreatherThan 10 ss)
    print "end"