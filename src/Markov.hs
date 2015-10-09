module Markov where

import Debug.Trace

data Ref = Ref {name::String, refs::[Ref]}

instance Show Ref where
    show (Ref n rs) = "Ref "++n++ " -> [|" ++ concat (map ( (++ "|") . name ) $ rs) ++ "]"

makeRefs :: [(String, [Int])] -> [Ref]

makeRefs x = let refs = (map buildRef) x
                 buildRef:: (String, [Int]) -> Ref
                 buildRef (name, indices) = Ref name (map transformIndexToRef indices)
                 transformIndexToRef :: Int -> Ref
                 transformIndexToRef i = refs !! i
                 in refs

x = [("First",[1]),("Second",[2]),("Third",[3,0]),("Fourth",[2])]

rs = makeRefs x

main = do
    print "start"
    print $ show rs
    print "end"