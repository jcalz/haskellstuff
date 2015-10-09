module Fix where

type MathFunction = Double -> Double

derivative :: MathFunction -> MathFunction
derivative f x = limit $ decreasingSequence $ difference f x

decreasingSequence :: MathFunction -> [Double]
decreasingSequence f = startFrom 1.0 where
    startFrom x = (f x) : startFrom (x/2.0)

difference :: MathFunction -> Double -> Double -> Double
difference f x h = (f (x + h) - f x) / h

limit :: [Double] -> Double
limit (m:ms@(n:ns)) 
    | m ~= n = m
    | otherwise = limit ms

maybeLimit :: Integer -> [Double] -> Maybe Double
maybeLimit 0 _ = Nothing
maybeLimit _ [] = Nothing
maybeLimit _ [x] = Nothing
maybeLimit l (m:ms@(n:ns)) 
    | m~=n = Just m
    | otherwise = maybeLimit (l-1) ms

(~=) :: Double -> Double -> Bool
x ~= y = abs(x - y) < 0.000000000000000000000000000001


foo = derivative (\x -> x+3)

root :: MathFunction -> Double
root f = limit (iterate modifiedNextNewton 0) where    
    nextNewton x =  (x - (f x)/(derivative f x))
    modifiedNextNewton x 
        | isInfinite (nextNewton x) = x + 0.276372672        
        | otherwise = nextNewton x 
        
roots :: MathFunction -> [Double]
roots f = (root f) : (roots (\x -> (f x)/(x - root f)))

fixedPointNewton :: MathFunction -> Double
fixedPointNewton f = root (\x -> f x - x)

fixedPointPlug :: MathFunction -> Maybe Double
fixedPointPlug f = maybeLimit 1000 $ (iterate f) 0

fixedPoint :: MathFunction -> Double
fixedPoint f = try (fixedPointPlug f) where
    try (Just x) = x
    try Nothing = fixedPointNewton f

main = do
    print "start"
--    print $ take 5 $ (\f -> iterate (\x -> (x - (f x)/(derivative f x)))) (\x -> (sin x) / x) 0.001
    print $ fixedPoint (\x -> 1 - (x*x))
    print "end"
