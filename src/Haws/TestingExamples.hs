module Haws.TestingExamples where

import Haws.TGraph
import Haws.GraphAlgorithms
import Haws.FGLTGraph
import Haws.TContext
import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS
import Data.Set
import Prelude hiding (pred,succ)

b1 :: Gr Char Char
b1 = mkGraph [(1,'a'),(2,'b'),(3,'c')] 
            [(1,2,'p'),(2,1,'q'),(2,3,'r'),(3,1,'s')]

e :: FGLTGraph Char
e = gEmpty

mb1 :: FGLTGraph Char
mb1 = insertTriples (fromList 
          [('a','p','b'),
           ('b','q','a'),
           ('b','r','c'),
           ('c','s','a')])
           e

b2 :: Gr Char Char
b2 = mkGraph [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')] 
            [(1,2,'p'),(2,1,'q'),(2,3,'r'),(3,1,'s'),(3,4,'t'),(3,5,'e'),(5,2,'b'),(1,3,'v')]

cormel :: Gr () ()
cormel = mkGraph 
           [(1,()),(2,()),(3,()),(4,()),(5,()),(6,())]
           [(1,2,()),(1,4,()),(2,5,()),(3,5,()),(3,6,()),(4,2,()),(5,4,()),(6,6,())]


i0 :: FGLTGraph Int
i0 = gEmpty

mcormel :: FGLTGraph Int
mcormel = insertTriples 
           (fromList [(1,0,2),(1,0,4),(2,0,5),(3,0,5),(3,0,6),(4,0,2),(5,0,4),(6,0,6)]) i0

mcormelCycle :: FGLTGraph Int
mcormelCycle = insertTriples
           (fromList [(1,0,2),(1,0,4),(2,0,5),(3,0,5),(3,0,6),(4,0,2),(5,0,4),(6,0,6),(6,0,1),(6,0,3)])
            i0
           
mb2 :: FGLTGraph Char
mb2 = insertTriples 
        (fromList [('c','t','d'),
                     ('c','u','e'),
                     ('a','v','c'),
                     ('e','w','b')]
        ) mb1

dfs1 = dfs [1] b1

dfs2 = dfs [1] b2

type Index = Int

data EValue a b = Node a | Edge b
 deriving (Show, Eq, Ord)

-- Simulate Erwig's graphs with my graphs
type EGraph a b = FGLTGraph (EValue a b)

ee :: FGLTGraph (EValue Char (String,Int))
ee = gEmpty

ee1 = insertTriples 
       (fromList 
       [(Node 'A',Edge ("AG",90), Node 'G'),
        (Node 'A',Edge ("AB",20), Node 'B'),
        (Node 'B',Edge ("BF",10), Node 'F'),
        (Node 'C',Edge ("CD",10), Node 'D'),
        (Node 'C',Edge ("CH",20), Node 'H'),
        (Node 'C',Edge ("CF",50), Node 'F'),
        (Node 'D',Edge ("DG",20), Node 'G'),
        (Node 'E',Edge ("EB",50), Node 'B'),
        (Node 'E',Edge ("EG",30), Node 'G'),
        (Node 'F',Edge ("FC",10), Node 'C'),
        (Node 'F',Edge ("FD",40), Node 'D')
       ])
      ee


mapEGraph :: (Ord a, Ord b, Ord c, Ord d,
              Show a, Show b, Show c, Show d) => (a -> c) -> (b -> d) -> EGraph a b -> EGraph c d
mapEGraph f g = gmapTGraph (\ctx -> emapCtx f g ctx)

emapCtx :: (Ord a, Ord b, Ord c, Ord d) => (a -> c) -> (b -> d) -> TContext (EValue a b) -> TContext (EValue c d)
emapCtx f g ctx =  ctx { node = emapNode f g (node ctx), 
                         succ = emapPairs f g (succ ctx),
                         pred = emapPairs f g (pred ctx),
                         rels = emapPairs f g (rels ctx) 
                       }

emapPairs :: (Ord a, Ord b, Ord c, Ord d) => (a -> c) -> (b -> d) -> 
                        Set(EValue a b, EValue a b) -> Set(EValue c d, EValue c d)
emapPairs f g = Data.Set.map (\(x,y) -> (emapNode f g x,emapNode f g y))

emapNode :: (a -> c) -> (b -> d) -> EValue a b -> EValue c d
emapNode f g (Node a) = Node (f a)
emapNode f g (Edge b) = Edge (g b)

-- Examples from paper
g1 :: FGLTGraph Char
g1 =   Ctx { node = 'a',pred = fromList [('c','s'),('b','q')], succ = fromList [('p','b')],rels = fromList []} `comp`
     ( Ctx { node = 'b', pred = fromList [], succ = fromList [('r','c')], rels= fromList [] } `comp`
     ( Ctx { node = 'c', pred = fromList [], succ = fromList [], rels = fromList [] } `comp`
     ( Ctx { node = 'p', pred = fromList [], succ = fromList [], rels = fromList [] } `comp`
     ( Ctx { node = 'q', pred = fromList [], succ = fromList [], rels = fromList [] } `comp`
     ( Ctx { node = 'r', pred = fromList [], succ = fromList [], rels = fromList [] } `comp`
     ( Ctx { node = 's', pred = fromList [], succ = fromList [], rels = fromList [] } `comp`
       gEmpty
     ))))))
    

