module Combinators where

--y :: ((a->b) -> (a->b)) -> (a -> b)
--y f = f (y f)

fac _ 0 = 1
fac f n = n * (f (n-1))

data Wrap a = Wrap {unwrap :: Wrap a -> a}

y :: (a->a) -> a
y f1 = (g (Wrap g)) where
    g x = (f1 xx1) where
        (xx1) = (unwrap x) x

y2 :: (a->b->a) -> (a->b->b) -> (a,b)
y2 f1 f2 = (g (Wrap g)) where
    g x = (f1 xx1 xx2, f2 xx1 xx2) where
        (xx1, xx2) = (unwrap x) x

y3 :: (a->b->c->a) -> (a->b->c->b) -> (a->b->c->c) -> (a,b,c)
y3 f1 f2 f3  = (g (Wrap g)) where
    g x = (f1 xx1 xx2 xx3, f2 xx1 xx2 xx3, f3 xx1 xx2 xx3) where
        (xx1,xx2,xx3) = (unwrap x) x

--yn :: 

mine m y 0 = 10
mine m y n = (0.75*(m (n-1))+0.25*(y (n-1)))

yours m y 0 = 0
yours m y n = (0.25*(m (n-1))+0.75*(y (n-1)))

(fmine, fyours) = y2 mine yours

ours = map (\n -> (n, fmine n, fyours n)) [0..]

main = do
    print "start"
    print $ take 15 ours
    print "end"
    