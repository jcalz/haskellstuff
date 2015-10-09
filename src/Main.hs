-- | Main entry point to the application.
module Main where

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a,b) : zip as bs

extend :: [a] -> [Maybe a]
extend as = map return as ++ nothings where
    nothings = Nothing : nothings

zipForever :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipForever as bs = zip (extend as) (extend bs)

zipRest :: [a] -> [b] -> ([(a,b)], Either [a] [b])
zipRest as [] = ([], Left as)
zipRest [] bs = ([], Right bs)
zipRest (a:as) (b:bs) = ((a,b):abs, rest) where (abs,rest) = zipRest as bs

--data TwoLists a b = NilNil | ConsCons a b (TwoLists a b) | ConsNil [a] | NilCons [b]

data Things a b c = None | Lefts a | Rights b | Both c

data ConsNil a = NilNil | ConsNil a (ConsNil a)

consNil :: [a] -> ConsNil a
consNil [] = NilNil
consNil (a:as) = ConsNil a (consNil as)

data Cons a b = Cons a b

data TwoLists a b = TwoLists (Things (ConsNil a) (ConsNil b) (Cons (a,b) (TwoLists a b)))

zippy :: [a] -> [b] -> TwoLists a b
zippy [] [] = TwoLists None
zippy as [] = TwoLists (Lefts (consNil as))
zippy [] bs = TwoLists (Rights (consNil bs))
zippy (a:as) (b:bs) = TwoLists (Both (Cons (a,b) (zippy as bs)))


-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"
