{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module ForceShow where

instance Show a where 
    show _ = "unshowable"

class Blop x where
    blorp :: x -> String


--instance (Blop x) => Show x where
  --  show = blorp
doo = shows 2

main = do
    print "hey"
    print 1
    print 3.5
    print (\a -> a)
    print "you"
    print ("this",[3, 2, 9.4],const)