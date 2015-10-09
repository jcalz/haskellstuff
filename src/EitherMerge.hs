module EitherMerge where

import Data.Bifunctor as Bifunctor

fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

eitherMerge :: (a->c) -> (b->c) -> (Either a b) -> c
--eitherMerge f g = fromEither . (Bifunctor.bimap f g) 
eitherMerge = ((fromEither .) .) . Bifunctor.bimap

main = do
    print "EitherMerge"
    print "End"