module Sinc where

x = iterate (/2) (pi/2)
y = iterate (\yk -> ((1+sqrt(yk))/2)) 0
z = zipWith (\x y -> x/(1-(y*y))) x y

main = do
    print "start"
    print $ take 30 $ y
    print "end"
