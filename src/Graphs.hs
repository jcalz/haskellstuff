{-# LANGUAGE RankNTypes #-}
module Graphs where

import Data.List
import Data.Distributive

data Graph a = Graph [Node a] [Edge a] deriving (Show)
data Edge a = Edge (Node a) (Node a) deriving Show
--instance (Eq a) => Eq (Edge a) where

undirectedEq :: (Eq a) => Edge a -> Edge a -> Bool
(Edge s1 d1) `undirectedEq` (Edge s2 d2) = (s1,d1)==(s2,d2) || (s1,d1)==(d2,s2)

directedEq :: (Eq a) => Edge a -> Edge a -> Bool
(Edge s1 d1) `directedEq` (Edge s2 d2) = (s1,d1)==(s2,d2)

edgeEq :: (Eq a) => EdgeType -> Edge a -> Edge a -> Bool
edgeEq Directed = directedEq
edgeEq Undirected = undirectedEq

flop :: Edge a -> Edge a
flop (Edge a b) = Edge b a

data Node a = Node a deriving (Eq, Show)

data EdgeType = Directed | Undirected

-- this function will represent a graph in the "minimal" way, with invariants like:
-- no repeat nodes
-- no repeat edges, including "reversed" edges
-- no loop edges
canon :: (Eq a) => EdgeType -> Graph a -> Graph a
canon eType (Graph ns es) = Graph ns' es' where
    ns' = nub $ ns ++ map (\(Edge x _) -> x) es ++ map (\(Edge _ x) -> x) es
    es' = nubBy (edgeEq eType) $ filter (\(Edge a b) -> (a /= b)) es

makeGraphTermGraph :: (Eq a) => EdgeType -> [a] -> [(a,a)] -> Graph a
makeGraphTermGraph et ns es = canon et $ Graph ns' es' where
    ns' = map Node ns
    es' = map (\(s,d)->(Edge (Node s) (Node d))) es

toGraphTerm :: Graph a -> ([a], [(a,a)])
toGraphTerm (Graph ns es) = (ns', es') where
    ns' = map (\(Node a)->a) ns
    es' = map (\(Edge (Node a) (Node b))->(a,b)) es

makeEdgeClauseGraph :: (Eq a) => EdgeType -> [(a,a)] -> Graph a
makeEdgeClauseGraph et = makeGraphTermGraph et []

toEdgeClause :: Graph a -> [(a,a)]
toEdgeClause = snd . toGraphTerm

makeAdjacencyListGraph :: (Eq a) => EdgeType -> [(a,[a])] -> Graph a
makeAdjacencyListGraph et adjs = makeGraphTermGraph et (map fst adjs) (concatMap (\(s,ds)->map (\d->(s,d)) ds ) adjs)

toAdjacencyList :: (Eq a) => Graph a -> [(a,[a])]
toAdjacencyList (Graph ns es) = toDirectedAdjacencyList (Graph ns (es ++ map flop es))

toDirectedAdjacencyList :: (Eq a) => Graph a -> [(a,[a])]
toDirectedAdjacencyList (Graph ns es) = map f ns where
    f (na@(Node a)) = (a, [b | (Edge (Node a') (Node b)) <- es, a'==a])

data HumanFriendlyElement a = N a | (:-) a a
instance (Show a) => Show (HumanFriendlyElement a) where
    show (N a) = "N "++(show a)
    show (a :- b) = (show a)++":-"++(show b)

makeHumanFriendlyGraph :: (Eq a) => EdgeType -> [HumanFriendlyElement a] -> Graph a
makeHumanFriendlyGraph et hfes = makeGraphTermGraph et bareNodes edges where
    bareNodes = [ a | (N a) <- hfes ]
    edges = [ (a,b) | (a :- b) <- hfes ]

toHumanFriendly :: (Eq a) => Graph a -> [HumanFriendlyElement a]
toHumanFriendly (Graph ns es) = map N bareNodes ++ map (\(x,y)->(x:-y)) edges where
    bareNodes = filter (not . (`elem` edgeNodes)) (map (\(Node a)->a) ns)
    edges = map (\(Edge (Node a) (Node b)) -> (a,b)) es
    edgeNodes = map fst edges ++ map snd edges

addEdge :: (Eq a) => EdgeType -> (Edge a) -> (Graph a) -> (Graph a)
addEdge et e@(Edge a b) (Graph ns es) = canon et $ Graph ns (e:es)

removeEdge :: (Eq a) => EdgeType -> (Edge a) -> (Graph a) -> (Graph a)
removeEdge et e (Graph ns es) = Graph ns (filter (not . (edgeEq et) e) es)

addNode :: (Eq a) => (Node a) -> (Graph a) -> (Graph a)
addNode n (Graph ns es) = canon Directed (Graph (n:ns) es)

removeNode :: (Eq a) => (Node a) -> (Graph a) -> (Graph a)
removeNode n (Graph ns es) = (Graph ns' es') where
    ns' = filter (/=n) ns
    es' = filter (\(Edge a b) -> (a /= n) && (b /= n)) es

{--instance Functor Node where
    fmap f (Node a) = Node (f a)

instance Functor Edge where
    fmap f (Edge a b) = Edge (fmap f a) (fmap f b)

instance Functor Graph where
    fmap f (Graph ns es) = Graph ((fmap.fmap) f ns) ((fmap.fmap) f es) where
--}

gmap :: (Eq b) => (EdgeType) -> (a->b) -> Graph a -> Graph b
gmap et f (Graph ns es) = canon et $ Graph ns' es' where
    ns' = map (\(Node a) -> Node (f a)) ns
    es' = map (\(Edge (Node a) (Node b)) -> Edge (Node (f a)) (Node (f b))) es

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f as = [b | (Just b) <- map f as ]

nextNodes :: (Eq a) => EdgeType -> Graph a -> Node a -> [Node a]
nextNodes Directed (Graph _ es) n = ns' where
    ns' = filterMap (\(Edge a b) -> if (a==n) then Just b else Nothing) (es)
nextNodes Undirected (Graph ns es) n = nextNodes Directed (Graph ns (es ++ map flop es)) n

prevNodes :: (Eq a) => EdgeType -> Graph a -> Node a -> [Node a]
prevNodes Directed (Graph _ es) n = ns' where
    ns' = filterMap (\(Edge a b) -> if (b==n) then Just a else Nothing) es 
prevNodes Undirected g n = nextNodes Undirected g n

goo = makeGraphTermGraph Undirected ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]

--findAcyclicPaths :: (Eq a) => Node a -> Node a -> Graph a -> [[Node a]]
--findAcyclicPaths _ _ (Graph _ [])  = []
--findAcyclicPaths s d g = concatMap f (adjacentNodes g s) where
--    f s' = map (s:) (findAcyclicPaths s' d (removeEdge (Edge s s') g)) 

findAcyclicPaths :: (Eq a) => EdgeType -> Node a -> Node a -> Graph a -> [[Node a]]
--findAcyclicPaths s d g = undefined
-- if the source and destination node are the same, return [[d]]
-- remove the first node from the list of acceptable nodes
-- get the intersection of the list of acceptable nodes and the nodes adjacent to the first node
-- for each of these acceptable adjacent nodes: 
--   get all the acyclic paths from the new node to the destination
-- concat the whole thing together
-- prepend the source node to each concatted list
findAcyclicPaths et s d g@(Graph ns _) = fap s d ns where
    fap s d ns = if (s==d) then [[d]] else paths where     
        ns' = filter (/= s) ns
        adj = filter (`elem` ns') (nextNodes et g s)
        subpaths = concatMap (\n -> fap n d ns') adj
        paths = map (s:) subpaths

findCycles :: (Eq a) => EdgeType -> Node a -> Graph a -> [[Node a]]
findCycles et n g = map (n:) (concatMap (\s -> findAcyclicPaths et s n g) (nextNodes et g n))
        

blop = findAcyclicPaths Directed (Node 1) (Node 4) (makeEdgeClauseGraph Directed [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)])

gd = makeEdgeClauseGraph Directed [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
gu = makeEdgeClauseGraph Undirected [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]

data Annotated a b = Annotated a b deriving Show
instance (Eq a) => Eq (Annotated a b) where
    (Annotated a _)==(Annotated a' _) = a==a'

isConnected :: (Eq a) => Graph a -> Bool
isConnected (Graph [] _) = True
isConnected (Graph [n] _) = True
isConnected g@(Graph (n:_) _) = case (nextNodes Undirected g n) of
    [] -> False
    ns -> isConnected (gmap Undirected (\n' -> if ((Node n') `elem` ns) then n else (Node n')) g)

gg = (addEdge Undirected (Edge (Node 5) (Node 2)) gu)

main = do
    print "start G4"
  --  print $ findAcyclicPaths Undirected (Node 1) (Node 2) gu
  --  print $ findCycles Undirected (Node 2) (gu)
    --print $ gmap Directed (\x -> Annotated x x) gd
    print $ gu
    print $ isConnected gu 
  --  print $ gmap Directed (\(Annotated x y) -> (Annotated (x `mod` 2 == 0) y)) $ gmap Directed (\x -> Annotated x x) gd
    print "done G4"
