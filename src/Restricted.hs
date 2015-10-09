{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RebindableSyntax #-}


module Restricted where

import Data.Set as Set
import GHC.Exts (Constraint)
import Prelude hiding (fmap, (>>=), return, (>>))
import qualified Prelude as Prelude
import qualified Control.Applicative as Applicative

class RestrictedFunctor f where
    type FunctorConstraint f a :: Constraint
    type FunctorConstraint f a = ()
    
    fmap :: (FunctorConstraint f a, FunctorConstraint f b) => (a -> b) -> (f a -> f b)


    

class (RestrictedFunctor a) => RestrictedApplicative a where
    type ApplicativeConstraint a t :: Constraint
    type ApplicativeConstraint a t = ()
    
    pure :: (ApplicativeConstraint a t) => t -> a t

    (<*>) :: (ApplicativeConstraint a (t -> u), ApplicativeConstraint a t, ApplicativeConstraint a u) => a (t -> u) -> a t -> a u
    infixl 4 <*>

    (*>) :: (ApplicativeConstraint a (t -> u -> u), ApplicativeConstraint a (u->u), ApplicativeConstraint a t, ApplicativeConstraint a u) => a t -> a u -> a u
    infixl 4 *>
    (*>) at au = (pure (const id)) <*> at <*> au    

    (<*) :: (ApplicativeConstraint a (t -> u -> t), ApplicativeConstraint a (u -> t), ApplicativeConstraint a t, ApplicativeConstraint a u) => a t -> a u -> a t
    infixl 4 <*
    (<*) at au = (pure const) <*> at <*> au

    
class (RestrictedApplicative m) => RestrictedMonad m where
    type MonadConstraint m a :: Constraint
    type MonadConstraint m a = ()

    return :: (MonadConstraint m a) => a -> m a

    (>>=) :: (MonadConstraint m a, MonadConstraint m b) => m a -> (a -> m b) -> m b 
    infixl 1 >>= 

    (>>) :: (MonadConstraint m a, MonadConstraint m b) => m a -> m b -> m b
    infixl 1 >>
    ma >> mb = ma >>= (const mb)    

    fail :: (MonadConstraint m a) => String -> m a
    fail = error

instance RestrictedFunctor Set where
    type FunctorConstraint Set a = (Ord a)
    fmap f = Set.foldr (Set.insert . f) Set.empty


instance RestrictedApplicative Set where
    type ApplicativeConstraint Set a = (Ord a)
    pure = Set.singleton
    af <*> at = Set.foldr (\f acc -> Set.union acc (fmap f at)) Set.empty af

instance RestrictedMonad Set where
    type MonadConstraint Set a = (Ord a)
    return = pure
    ma >>= f = Set.foldr (\a acc -> Set.union acc (f a)) Set.empty ma 

instance RestrictedFunctor IO where
    fmap = Prelude.fmap
instance RestrictedApplicative IO where
    pure = Applicative.pure
    (<*>) = (Applicative.<*>)
instance RestrictedMonad IO where
    return = Prelude.return
    (>>=) = (Prelude.>>=)

intset = Set.fromList [1..100]

instance Eq (Integer -> Integer) where
    f == g = all (\i -> (f i == g i)) [-100..100]
instance Ord (Integer -> Integer) where
    f <= g = (Prelude.map f [-100..100]) <= (Prelude.map g [-100..100])
main = do
    print "hey"
    print $ (Set.fromList [(* 2),(* 3)]) <*> (Set.fromList [3, 9, 12] :: Set.Set Integer)
    print "you"