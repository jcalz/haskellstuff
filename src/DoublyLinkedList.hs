module DoublyLinkedList where

import Debug.Trace
import Control.Applicative

class DoublyLinkedList l where
 fromList :: [a] -> l a
 cur :: l a -> Maybe a
 next :: l a -> Maybe (l a)
 prev :: l a -> Maybe (l a)

 start :: l a -> l a
 start l = case (prev l) of 
     Nothing -> l
     Just p -> start p
     
 end :: l a -> l a
 end l = case (next l) of
     Nothing -> l
     Just n -> end n

 toList :: l a -> [a]
 toList l = walk (start l) where
     walk m = prependCur rest where
         prependCur r = case (cur m) of
             Nothing -> r
             Just x -> x : r
         rest = case (next m) of
             Nothing -> []
             Just n -> walk n

 index :: l a -> DoublyLinkedIndex
 index l = case (next l) of
    Nothing -> EndIndex
    _ -> case (prev l) of
        Nothing -> StartIndex
        Just p -> case (index p) of
            StartIndex -> Index 0
            Index n -> Index (n+1)

 len ::  l a -> Integer
 len l = walk (start l) where
     walk m = first + rest where
         first = case (cur m) of
             Nothing -> 0
             _ -> 1
         rest = case (next m) of
             Nothing -> 0
             Just n -> walk n

 get :: DoublyLinkedIndex -> l a -> l a
 get StartIndex = start
 get EndIndex = end 
 get (Index i)
    | (i<0) = start
    | otherwise = \l -> case (iterate (>>= next) (Just (start l))) !! (1 + fromInteger i) of
        Just m -> m
        Nothing -> end l

 insertBefore :: l a -> a -> l a
 insertBefore l a = get ind (fromList (prevList ++ [a] ++ curAndNextList)) where
     ind = Index curInd
     fullList = toList l
     curInd = case (index l) of 
         StartIndex -> error "cannot insert before start"
         Index i -> i
         EndIndex -> len l
     (prevList, curAndNextList) = splitAt (fromInteger curInd) fullList 
    
 insertAfter :: l a -> a -> l a
 insertAfter l a = get ind (fromList (prevAndCurList ++ [a] ++ nextList)) where
     ind = Index curInd
     fullList = toList l
     curInd = 1 + case (index l) of 
         StartIndex -> -1
         Index i -> i
         EndIndex -> error "cannot insert after end"
     (prevAndCurList, nextList) = splitAt (fromInteger curInd) fullList 

 remove :: l a -> l a
 remove l = get ind (fromList (prevList ++ nextList)) where
     ind = Index curInd
     fullList = toList l
     curInd = case (index l) of 
         StartIndex -> error "cannot remove start"
         Index i -> i
         EndIndex -> error "cannot remove end"
     (prevList, cur:nextList) = splitAt (fromInteger curInd) fullList

data DoublyLinkedIndex = StartIndex | Index Integer | EndIndex deriving Show

data DList a = Start (DList a) | Mid (DList a) a (DList a) | End (DList a)

instance (Show a) => Show (DList a) where 
        show (Start n) = "## Start ## <=> "++ nextshow n
        show (Mid p a n) = prevshow p ++ "## " ++ show a ++ " ## <=> " ++ nextshow n
        show (End p) = prevshow p ++ " ## End ##"

prevshow :: (Show a) => DList a -> String
prevshow (Start _) = "Start <=> "
prevshow (Mid p a _) = prevshow p ++ " " ++ show a ++ " <=> "
prevshow (End p) = prevshow p ++ " End"

nextshow :: (Show a) => DList a -> String
nextshow (Start n) = "Start <=> " ++ nextshow n
nextshow (Mid _ a n) = show a ++ " <=> " ++ nextshow n
nextshow (End _) = "End"





instance DoublyLinkedList DList where
    fromList xs = let this = Start (buildDList this xs) in this where 
        buildDList :: DList a -> [a] -> DList a
        buildDList p [] = End p
        buildDList p (x:xs) = let this = Mid p x (buildDList this xs) in this

    cur (Start _) = Nothing
    cur (End _) = Nothing
    cur (Mid _ a _) = Just a

    next (Start n) = Just n
    next (Mid _ _ n) = Just n
    next (End _) = Nothing

    prev (Start _) = Nothing
    prev (Mid p _ _) = Just p
    prev (End p) = Just p


data TwoLists a = StartTL [a] | MidTL [a] a [a] | EndTL [a] deriving Show

toRevList (StartTL n) = reverse n
toRevList (MidTL p x n) = (reverse n)++(x:p)
toRevList (EndTL p) = p


instance DoublyLinkedList TwoLists where
    fromList = StartTL

    cur (MidTL _ x _) = Just x
    cur _ = Nothing

    next (StartTL []) = Just (EndTL [])
    next (StartTL (n:ns)) = Just (MidTL [] n ns)
    next (MidTL p x []) = Just (EndTL (x:p))
    next (MidTL p x (n:ns)) = Just (MidTL (x:p) n ns)
    next (EndTL _) = Nothing

    prev (EndTL []) = Just (StartTL [])
    prev (EndTL (p:ps)) = Just (MidTL ps p [])
    prev (MidTL [] x n) = Just (StartTL (x:n))
    prev (MidTL (p:ps) x n) = Just (MidTL ps p (x:n))
    prev (StartTL _) = Nothing

    toList (StartTL n) = n
    toList (MidTL p x n) = (reverse p)++(x:n)
    toList (EndTL p) = reverse p    

    start = (StartTL . toList)

    end = (EndTL . toRevList)

    index (StartTL _) = StartIndex
    index (MidTL p _ _) = Index $ toInteger $ length p
    index (EndTL _) = EndIndex

    len (StartTL n) = toInteger $ length n 
    len (MidTL p _ n) = toInteger (length p + length n + 1)
    len (EndTL p) = toInteger $ length p

    -- get, leave default

    insertBefore (StartTL _) _ = error "can't insert before start"
    insertBefore (MidTL p c n) x = MidTL p x (c:n)
    insertBefore (EndTL p) x = MidTL p x []

    insertAfter (StartTL n) x = MidTL [] x n
    insertAfter (MidTL p c n) x = MidTL (c:p) x n
    insertAfter (EndTL _) _ = error "can't insert after end"

    remove (StartTL _) = error "can't remove start"
    remove (MidTL (p:ps) c n) = MidTL ps p n
    remove (MidTL [] c n) = StartTL n
    remove (EndTL _) = error "can't remove end"

--foo :: DList Integer
foo = fromList [1,2,3] :: TwoLists Integer

main = do
    print "start"
--    print $ take 7 $ map (fmap index) (iterate (>>= next) (Just (start foo)))
--    print $ take 7 $ (iterate (fmap (\l -> case (cur l) of {Nothing -> Nothing, Just x -> remove l})) (next foo))
    print "done"