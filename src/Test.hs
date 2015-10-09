module Test where

import ShortestPath

adjust :: Int -> (a -> a) -> [a] -> [a]
adjust i f as = take i as ++ (f (as !! i)) : drop (i+1) as

letterChange :: Int -> String -> [String]
letterChange i s = map (f i s) [-20 .. 20] where
    f i s d = adjust i (toEnum . (+ d) . fromEnum) s

wordChange :: String -> [String]
wordChange s = concatMap (flip letterChange s) [0..length s - 1]


main = print $ shortestPath wordChange (== "COW") "DOG"
