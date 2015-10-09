module Stuff where

import Control.Applicative

data MessageValue a = MessageValue String a deriving Show

instance Functor MessageValue where
    fmap f (MessageValue s a) = MessageValue s (f a)

instance Applicative MessageValue where
    pure a = MessageValue "" a
    (MessageValue fs f) <*> (MessageValue as a) = (MessageValue (fs++" "++as) (f a)) 

instance Monad MessageValue where
    (MessageValue s a) >>= f = (MessageValue (s++" "++bs) b) where (MessageValue bs b) = f a
    return = pure

a = do
    x <- MessageValue "assigning x" 2
    y <- MessageValue "assigning y" 3
    return 0

main = do
    print "start"
    print $  a
    print "end"
    

