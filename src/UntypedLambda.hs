{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module UntypedLambda where

data L a = L ( L a -> L a) | V a

deval (V a) = a
($$) (L f) x = f x

n0 = L (\f -> L (\x -> x))
s = L (\n -> L (\f -> (L (\x -> f $$ (n $$ f $$ x)))))

n1 = s $$ n0
n2 = s $$ n1
n3 = s $$ n2

true = L (\f -> L (\s -> f))
false = L (\f -> L (\s -> s))


vf :: (a -> a) -> L a
vf f = L (\(V x) -> (V (f x)))

eta :: L a -> L a
eta l = L (\x -> l $$ x)

fromChurchNumeral :: L Integer -> Integer
fromChurchNumeral n = deval $ n $$ (vf succ) $$ (V 0)

fromChurchBoolean :: L Bool -> Bool
fromChurchBoolean b = deval $ b $$ (V True) $$ (V False)

main = do
    print "Start UntypedLambda"
    print $ fromChurchNumeral $ eta (eta n3)
    print $ fromChurchBoolean $ (eta true)
    print "End UntypedLambda"

