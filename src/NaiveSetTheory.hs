module NaiveSetTheory where

import Prelude hiding ((-),(<),(<=),null, succ)
import qualified Prelude as P

import Data.List hiding (null)

data Set = Set {elements :: [Set]} deriving Eq

instance Show Set where
    show (Set as) = '{': showList as ++ "}" where
        showList [] = ""
        showList [a] = show a
        showList (a:as) = show a ++ "," ++ showList as
    

(∈) :: Set -> Set -> Bool
x ∈ s = x `elem` (elements s) 

(∉) :: Set -> Set -> Bool
(∉) x = not . (∈) x

(∪) :: Set -> Set -> Set
(Set as) ∪ (Set bs) = Set (nub (as ++ bs))

(∩) :: Set -> Set -> Set
(Set as) ∩ (Set bs) = Set (filter (`elem` bs) as)

(-) :: Set -> Set -> Set
(Set as) - (Set bs) = Set (filter (not . (`elem` bs)) as)

(∅) :: Set
(∅) = Set []

null = (∅)


-- let's generate von neumann ordinals or something?

setOf :: Set -> Set
setOf a = Set [a]

succ :: Set -> Set
succ a = a ∪ (setOf a)

(<) :: Set -> Set -> Bool
(<) = (∈)

(<=) :: Set -> Set -> Bool
a <= b = a < b || a == b



main = do
    print "NaiveSetTheory start"
    print $ succ (succ (succ null))
    print "end"