{-# LANGUAGE RankNTypes #-}
module ChurchEncoding where

import Control.Applicative
import Control.Monad

data ListC a = ListC {foldC :: forall r. (a -> r -> r) -> r -> r}

demoListC = ListC (\cons nil -> cons "first" (cons "second" (cons "third" nil)))

toList :: ListC a -> [a]
toList l = foldC l (:) []

fromList :: [a] -> ListC a
fromList as = foldr (consC) nilC as

instance (Show a) => Show (ListC a) where
  show l = "ListC " ++ (show $ toList l)

nilC :: ListC a
nilC = ListC (\ cons nil -> nil)

consC :: a -> ListC a -> ListC a
consC a l = ListC (\cons nil -> cons a (foldC l cons nil))


uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (a:az) = Just (a, az) 
    
unconsC :: ListC a -> Maybe (a, ListC a)
unconsC l = foldC l (\a l -> Just (a, reconsC l)) Nothing where
        reconsC :: Maybe (a, ListC a) -> ListC a
        reconsC Nothing = nilC
        reconsC (Just (a,l)) = consC a l

        
liveDangerously :: Maybe a -> a
liveDangerously (Just a) = a
liveDangerously Nothing = error "lived dangerously, died young"

appendC :: ListC a -> ListC a -> ListC a
appendC l m = foldC l consC m

concatC :: ListC (ListC a) -> ListC a
concatC ll = foldC ll (appendC) nilC

instance Functor (ListC) where
    fmap f l = ListC (\cons nil -> foldC l (cons . f) (nil))

instance Applicative (ListC) where
    pure a = ListC (\cons nil -> cons a nil)
    (<*>) lop la = concatC (fmap (\op -> fmap op la) lop)

instance Monad (ListC) where
    (>>=) la op = concatC (fmap op la) 
    return = pure

gronk = ListC (\cons nil -> cons 1 (foldC gronk cons nil))

-- r cons nil = cons 1 (r cons nil)
    
main = do
    print "["
    print (fmap (*3) (fromList [4,5,6]))
    print gronk
    print "]"