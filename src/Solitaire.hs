-- | Main entry point to the application.
module Solitaire where

import Data.List

data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq, Ord, Enum)

data Suit = Hearts | Clubs | Diamonds | Spades deriving (Eq, Ord, Enum)

data Color = Red | Black deriving (Eq, Ord, Enum)

suitColor :: Suit -> Color
suitColor Hearts = Red
suitColor Clubs = Black
suitColor Diamonds = Red
suitColor Spades = Black


instance Show Value where
    show v = case v of Ace -> "A" ; Two -> "2" ; Three -> "3" ; Four -> "4" ; Five -> "5" ; Six -> "6" ; Seven -> "7"
                       Eight -> "8" ; Nine -> "9" ; Ten -> "T" ; Jack -> "J" ; Queen -> "Q" ; King -> "K"


instance Show Suit where
    show Hearts = "♡"
    show Clubs = "♣"
    show Diamonds = "♢"
    show Spades = "♠"


data Card = Card Value Suit deriving (Eq, Ord)
instance Show Card where
    show (Card v s) = show v ++ show s





--data Column = Column {faceDown :: [Card], faceUp :: [Card]}

type TopFaceUpCard = Card
type FaceUpCard = Card
type FaceDownCard = Card

data Column = EmptyColumn | Column TopFaceUpCard [FaceUpCard] [FaceDownCard]

columnAccepts :: Column -> Card -> Bool
columnAccepts EmptyColumn (Card v _) = (v == King)
columnAccepts (Column (Card v s) _ _) (Card v' s') = (v == succ v') && (suitColor s /= suitColor s')

stackColumn :: Column -> [Card] -> Column
stackColumn c [] = c
stackColumn EmptyColumn (c:cs) = Column c cs []
stackColumn (Column t us ds) (c:cs) = Column c (cs ++ t:us) ds

splitColumnAt :: Int -> Column -> Maybe ([Card],Column)
splitColumnAt _ EmptyColumn = Nothing
--splitColumnAt i (Column t us ds) = if (i>length us) then Nothing else Just (t : take (i-1) us , dropColumn i us)


columnMove :: (Column, Column) -> Maybe (Column, Column)
columnMove (EmptyColumn, _) = Nothing
--columnMove (Column t us ds, dest) = 




--stacks :: Column -> [([Card],Column)]  
--stacks EmptyColumn = []
--stacks (Column u us ds) = ([u],Column) : undefined

data Board = Board {tableau :: [Column], foundation :: ([Card],[Card],[Card],[Card]), deck :: [Card], waste :: [Card]} -- yup


(!.) :: (b -> [c]) -> (a -> [b]) -> (a -> [c])
f !. g = (>>= f) . g

assert :: Bool -> [()]
assert True = [()]
assert False = []

blarp = do
    a <- [1..9]
    b <- [1..9]
    c <- [0..9]
    let num = (10*a+b)
        den = (10*b+c)        
        in do
            assert (num/=den)
            assert (num*c == den*a)
            return (num,den)

-- generate and test



            
-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Start Solitaire"
    print blarp
    putStrLn "End Solitaire"
