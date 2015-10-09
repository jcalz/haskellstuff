{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Main entry point to the application.
module Main where

import Control.Monad
import Debug.Trace
import Control.Applicative

replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' i m = 
    if (i<=0) then 
        return []
    else do
        a <- m
        as <- replicateM' (i-1) m 
        return (a:as)

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' = ((.).(.)) concat map

class Hashable a where
    type HashType a :: *
    hash :: (Enum (HashType a), Bounded (HashType a)) => a -> (HashType a)


hashLift :: (Enum a, Bounded a) => (Int -> Int) -> (a -> a)
hashLift f e = toEnum ((f (x - x0) `mod` h) + x0) where 
    [_,lb,ub] = [e, minBound, maxBound]
    x = fromEnum e
    x0 = fromEnum lb
    h = fromEnum ub - x0


--data MonadCompose m x = MonadCompose (x -> m x) (forall a b c. ((b -> m c) -> (a -> m b) -> (a -> m c)))

--instance (Monad m) => (Monad (MonadCompose m)) where
--    return x = 

class MonadComposer m where
    oIdentity :: x -> m x
    oI :: x -> m x
    oI = oIdentity
    oCompose :: (b -> m c) -> (a -> m b) -> (a -> m c)
    (>.>) :: (b -> m c) -> (a -> m b) -> (a -> m c)
    (>.>) = oCompose

newtype MonadC m x = MonadC {unMonadC :: m x}

instance (MonadComposer m) => (Monad (MonadC m)) where
    return x = MonadC (oIdentity x)
    (MonadC ma) >>= f = MonadC (oCompose (unMonadC . f) (const ma) ())

instance (MonadComposer m) => (Applicative (MonadC m)) where
    pure = return
    mf <*> ma = mf >>= (\f -> ma >>= return . f)

instance (MonadComposer m) => (Functor (MonadC m)) where
    fmap f ma = ma >>= (return . f)

foo :: (MonadComposer m) => Bool -> (c -> m d) -> (b -> m c) -> (a -> m b) -> (a -> m d)
foo True f g h = (f >.> (g >.> h))
foo False f g h = (f >.> g) >.> h

goo :: (MonadComposer m) => Bool -> (a -> m b) -> (a -> m b)
goo True f = f >.> oI
goo False f = oI >.> f

front2 :: (a1 -> a2 -> r) -> (a2 -> a1 -> r)
front2 = flip

front3 :: (a1 -> a2 -> a3 -> r) -> (a3 -> a1 -> a2 -> r)
front3 f a3 a1 a2 = f a1 a2 a3

data Void

data N n = (n ~ Void) => Z | forall m. (n ~ N m) => S (N m)

{--class Fun n where
    type F n
    type NArgs n
    
instance Fun (a,()) where
    type F (a,()) = a
    type NArgs (a,()) = N Void

instance (Fun (b,f)) => Fun (a,(b,f)) where
    type F (a,(b,f)) = a -> F (b,f)
    type NArgs (a,(b,f)) = N (NArgs (b,f))
--}

type family ReturnValue f where
    ReturnValue (a -> b) = ReturnValue b
    ReturnValue b = b

type family LastArg f where
    LastArg (a -> (b -> c)) = LastArg (b -> c)
    LastArg (a -> b) = a

type family FirstArg f where
    FirstArg (a -> b) = a

type family RemoveLastArg f where
    RemoveLastArg (a -> (b -> c)) = a -> RemoveLastArg (b -> c)
    RemoveLastArg (a -> b) = b

type family FrontLastArg f where
    FrontLastArg f = LastArg f -> RemoveLastArg f

type family FunctionType f where
    FunctionType (a -> b -> c) = MultiArg
    FunctionType (a -> b) = OneArg
    FunctionType a = NoArg

data MultiArg = MultiArg
data OneArg = OneArg
data NoArg = NoArg

--class FrontLastable ft where   
--    frontLast :: FunctionType f -> f -> FrontLastArg f




sfoo :: (a -> b) -> FrontLastArg (a -> b)
sfoo f z = undefined 

gorp :: Char -> Int -> Bool
gorp = undefined

jork = sfoo gorp

flip' :: (a -> b -> Int) -> FrontLastArg (a -> b -> Int)
flip' = flip

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "start"
    --print $ map ((toHash::(Int -> Int)) . (fromHash::(Int->Int)) ) [1,2,3,4,5]
  --  print $ hash [-33,999,99,19::Int]
    --print $ take 100 $ concatMap' (`replicateM'`[0,1]) [1..]
    putStrLn "end"

