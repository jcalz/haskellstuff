{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module List where

import Debug.Trace
import Data.List

unsafe :: Maybe a -> a
unsafe (Just a) = a
unsafe Nothing = error "unsafe unmaybe failed"

class (List l) where
    nilL :: l a
    consL :: a -> l a -> l a
    getL :: Integer -> l a -> Maybe a
    lengthL :: l a -> Maybe Integer
    mapL :: (a->b) -> l a -> l b
    appendL :: l a -> l a -> l a
    unconsL :: l a -> Maybe (a, l a)
    safeHeadL :: l a -> Maybe a
    safeHeadL = fmap fst . unconsL
    safeTailL :: l a -> Maybe (l a)
    safeTailL = fmap snd . unconsL
    headL :: l a -> a
    headL = unsafe . safeHeadL
    tailL :: l a -> l a
    tailL = unsafe . safeTailL
    convertL :: (List l') => l' a -> l a
    convertL l = case (unconsL l) of
        Nothing -> nilL
        Just (h, t) -> consL h (convertL t)
    takeL :: Integer -> l a -> l a
    takeL n l = case (n<=0, unconsL l) of 
        (True, _) -> nilL
        (False, Nothing) -> nilL
        (False, Just (h, t)) -> consL h (takeL (n-1) t)
    cycleL :: l a -> l a
    cycleL l = let c = appendL l c in c

instance (List []) where
    nilL = []
    consL = (:)
    getL _ [] = Nothing
    getL 0 (a:_) = Just a
    getL n (_:as) = getL (n-1) as
    lengthL = Just . toInteger . length
    mapL = map
    appendL = (++)
    unconsL [] = Nothing
    unconsL (a:as) = Just (a,as)


newtype FuncList a = FuncList (Integer -> Maybe a)
-- rely on FuncList implementation to have this feature
-- If (FuncList f) is a FuncList, then if f n is Nothing, then so is f (n+1).

instance List FuncList where
    nilL = FuncList (const Nothing)
    consL a (FuncList f) = FuncList (\i -> if (i==0) then (Just a) else f (i-1))
    getL i (FuncList f) = f i
    lengthL (FuncList f) = trace "lengthing" $ find (not . isSomething . f) [0..]
    mapL g (FuncList f) = FuncList (fmap g . f)
    appendL fl@(FuncList f) (FuncList g) = 
           FuncList (\i -> case (f i, lengthL fl) of
               (Just _, _) -> f i
               (Nothing, Just l) -> g (i - l))
    unconsL (FuncList f) = case (f 0) of 
        (Just a) -> Just (a, FuncList (f . (+1)))
        Nothing -> Nothing
    cycleL l@(FuncList f) = trace "cycling" $ FuncList (\i -> case (f i) of 
        (Just a) -> (trace "just" $ Just a)
        (Nothing) -> trace "nothing" $ f (i `mod` (unsafe (lengthL l)))
        )
    takeL n (FuncList f) = FuncList (\i -> if (i>=n) then Nothing else f i)

instance (Show a) => Show (FuncList a) where
    show l = "FuncList " ++ show (map id (convertL l))

isSomething :: Maybe a -> Bool
isSomething Nothing = False
isSomething _ = True
    
makeFuncList :: [a] -> FuncList a
makeFuncList as = FuncList (\i -> if (i < toInteger (length as)) then (Just $ as !! (fromInteger i)) else Nothing)

main = do
    print "["
    print (takeL 222 $ cycleL $ makeFuncList [1..9]) 
--    print (getL 5 $ appendL (makeFuncList ["this","that","the","other","thing"]) (makeFuncList ["this","that"]))
    print "]"