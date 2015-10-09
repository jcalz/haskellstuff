module HistogramSizing where

data Span = Span {min::Double, max::Double} deriving Show

data Digit = One | Two | Five deriving Show

digVal :: Digit -> Integer
digVal One = 1
digVal Two = 2
digVal Five = 5

humanCeil :: Double -> (Digit,Integer)
humanCeil x = di where
    scale = floor (log x / log 10)
    mant = x / (10 ^^ scale)
    di 
        | x==0 = (One, 0)
        | mant==1.0 = (One, scale)
        | mant<=2.0 = (Two, scale)
        | mant<=5.0 = (Five, scale)
        | otherwise = (One, scale+1)

prevWidth :: (Digit,Integer) -> (Digit, Integer)
prevWidth (One, s) = (Five, s-1)
prevWidth (Two, s) = (One, s)
prevWidth (Five, s) = (Two, s)

widthNum :: (Digit, Integer) -> Double
widthNum (d, i) = (fromInteger $ digVal d) * ((10.00 ^^ i))
 

initWidth (Span min max) = humanCeil (max - min)

data BinSpecs = BinSpecs {leftEdge :: Double, rightEdge :: Double, binWidth :: Double, numbins :: Integer} deriving Show

binSpecs (Span min max) width = BinSpecs (width*fromInteger left) (width*fromInteger right) width num where
    left' = floor (min/width)
    right' = ceiling (max/width)
    num' = right' - left'
    (num, left, right)
        | num'==0 = (1, left', right'+1)
        | otherwise = (num', left', right')

widths span = map (widthNum) $ iterate prevWidth (initWidth span)

allBinSpecs span = takeWhile ((< 6000000) . numbins) $ map (binSpecs span) (widths span)

main = do
    print "start"
    print $  map ((\x -> (log (fromInteger x) / log 10)) . numbins) $ allBinSpecs (Span 1.23456 99.87654)
    print "end"