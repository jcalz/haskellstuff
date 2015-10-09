module AutoDiff where

data AutoDiff a = AutoDiff a a deriving Show

instance (Num a) => Num (AutoDiff a) where
    (AutoDiff dx x) + (AutoDiff dy y) = (AutoDiff  (dx+dy) (x+y))
    (AutoDiff dx x) * (AutoDiff dy y) = (AutoDiff  (x*dy + y*dx) (x*y))
    abs (AutoDiff dx x) = (AutoDiff (dx * signum x) (abs x) )
    signum (AutoDiff dx x) = (AutoDiff 0 (signum x) )
    fromInteger i = (AutoDiff  0 (fromInteger i))
    negate (AutoDiff dx x) = (AutoDiff  (-dx) (-x))

instance (Fractional a) => Fractional (AutoDiff a) where
    fromRational r = (AutoDiff 0  (fromRational r))
    recip (AutoDiff dx x) = AutoDiff (-dx / ((recip x)*(recip x))) (recip x) 

instance (Floating a) => Floating (AutoDiff a) where
    pi = (AutoDiff 0  pi)
    exp (AutoDiff du u) = (AutoDiff (du * exp u) (exp u) )
    log (AutoDiff du u) = (AutoDiff  (du / u) (log u))
    sin (AutoDiff du u) = (AutoDiff (du * cos u) (sin u) )
    cos (AutoDiff du u) = (AutoDiff  (-du * sin u) (cos u))
    asin (AutoDiff du u) = (AutoDiff  (du / sqrt(1 - u*u)) (asin u))
    acos (AutoDiff du u) = (AutoDiff  (-du / sqrt(1 - u*u)) (acos u))
    atan (AutoDiff du u) = (AutoDiff  (du / (1 + u*u)) (atan u))
    sinh a = (exp a - (exp (-a)))/2
    cosh a = (exp a + (exp (-a)))/2
    asinh a = log( a + sqrt(a*a + 1) ) 
    acosh a = log (a + (sqrt(a - 1) * sqrt(a + 1)))
    atanh a = (log (1+a) - log (1-a))/2

x = \a -> AutoDiff 1 a

data Deriv a = Deriv a
data DerivValue a = DerivValue (Deriv a) a

deriv :: (Num a) => (AutoDiff a -> AutoDiff a) -> (a -> a)
deriv f x = (\(AutoDiff d v) -> d) (f (AutoDiff 1 x))

pow :: (Floating a) => a -> a -> a
pow b e = exp (e * (log b))

main = do
    print "<"
    print $ (deriv (\x -> x * log x)) (exp 1)
    print ">"