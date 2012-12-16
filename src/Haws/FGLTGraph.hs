module Haws.FGLTGraph where 

{- | TGraph implementation using standard Marting Erwig FGL library
-}
import qualified Data.Graph.Inductive as FGL
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Haws.TGraph
import Haws.TContext
import Haws.Subst
import Prelude hiding (succ,pred) -- Conflict with our succ/pred definitions

data ValueGraph a = Value { 
        grNode :: FGL.Node, 
        edges  :: Set.Set (a,a)
 } deriving Show
                
-- | A graph 'a' is a list of nodes, a real graph 'Gr' of 'Int', 
-- and maps from nodes and edges to those 'Int' values.
data FGLTGraph a = FGLTGraph {
  graph :: FGL.Gr a a,
  nodeMap :: Map.Map a (ValueGraph a)
} deriving Show
  

instance TGraph FGLTGraph where 
  gEmpty = emptyFGL
  decomp = decompFGL 
  nodes = nodesFGL
  insertTriple = insertTripleFGL
  insertNode = insertNodeFGL

showFGLTGraph :: (Show a) => FGLTGraph a -> String
showFGLTGraph gr = show gr

emptyFGL :: FGLTGraph a
emptyFGL = FGLTGraph {
 graph = FGL.empty,
 nodeMap = Map.empty
}

containsFGL :: (Ord a) => a -> FGLTGraph a -> Bool
containsFGL n gr = Map.member n (nodeMap gr)

decompFGL :: (Show a, Ord a) => 
        a -> FGLTGraph a -> Maybe (TContext a, FGLTGraph a)
decompFGL n gr = if (containsFGL n gr) then 
 let grn = getNodeFGL n gr
     dec = FGL.match grn (graph gr)
 in case dec of 
    (Just (preds,_,_,succs),grdec) ->
        Just (Ctx { 
                node = n,
                succ = prepareAdj succs gr,
                pred = prepareAdj preds gr,
                rels = edges ((nodeMap gr) Map.! n)
              },
              FGLTGraph {
                graph = grdec,
                nodeMap = Map.map (deleteEdgesWithNode n) (Map.delete n (nodeMap gr))
              })
    (_,_) -> Nothing
 else Nothing

deleteEdgesWithNode :: Ord a => a -> ValueGraph a -> ValueGraph a
deleteEdgesWithNode n v = v { edges = Set.filter (\(x,y) -> not (x == n || y == n)) (edges v)}

prepareAdj :: Ord a => [(a,FGL.Node)] -> FGLTGraph a -> Set.Set (a,a)
prepareAdj ls gr = Set.fromList (map (\(x,y) -> (x,fromJust (FGL.lab (graph gr) y))) ls)

nodesFGL :: (Ord a) => FGLTGraph a -> Set.Set a
nodesFGL gr = Map.keysSet (nodeMap gr)

-- | generate a new node in a 'Gr' graph
genNode :: (FGL.Graph gr) => gr a b -> FGL.Node
genNode gr = head (FGL.newNodes 1 gr)

getNodeFGL :: (Show a, Ord a) => a -> FGLTGraph a -> FGL.Node
getNodeFGL n gr = grNode $ fromJust $ Map.lookup n (nodeMap gr)

insertGetNodeFGL :: (Show a, Ord a) => a -> FGLTGraph a -> (FGLTGraph a,FGL.Node)
insertGetNodeFGL n gr = 
 if containsFGL n gr then (gr,getNodeFGL n gr)
 else
  let grn = genNode (graph gr)
  in (gr { graph = FGL.insNode (grn,n) (graph gr),
          nodeMap = Map.insert n (Value { grNode = grn, edges = Set.empty }) 
                               (nodeMap gr)
       },
      grn)
      
insertNodeFGL :: (Show a, Ord a) => a -> FGLTGraph a -> FGLTGraph a
insertNodeFGL n gr = fst (insertGetNodeFGL n gr)

insertTripleFGL :: (Show a, Ord a) => (a,a,a) -> FGLTGraph a -> FGLTGraph a
insertTripleFGL (x,p,y) gr = 
 let (gr1,nx) = insertGetNodeFGL x gr 
     (gr2,np) = insertGetNodeFGL p gr1
     (gr3,ny) = insertGetNodeFGL y gr2
 in 
  gr {
   graph = FGL.insEdge (nx,ny,p) (graph gr3),
   nodeMap = Map.adjust (addEdge (x,y)) p (nodeMap gr3)
  }
  
addEdge (x,y) v = v { edges = Set.insert (x,y) (edges v) }


emptyFGLInt :: FGLTGraph Int
emptyFGLInt = emptyFGL

ex1 :: FGLTGraph Int
ex1 = insertNodeFGL 1 emptyFGLInt

ex2 :: FGLTGraph Int
ex2 = insertTripleFGL (2,3,4) ex1

-- test unify
data TestNode = C Int
              | V String
  deriving (Eq,Show,Ord)
  
instance VarCheck TestNode where
  varCheck (C _) = Nothing
  varCheck (V x) = Just x
          
            
t1 :: FGLTGraph TestNode
t1 = emptyFGL

t2 = insertTripleFGL (C 1, C 2, V "x") $
     insertTripleFGL (V "x", C 2, C 3) $
     emptyFGL
     
t3 = insertTripleFGL (V "y", C 2, C 4) $
     insertTripleFGL (C 4, C 2, C 3) $
     emptyFGL

t12x = insertTripleFGL (C 1, C 2, V "x") $
     emptyFGL

t123 = insertTripleFGL (C 1, C 2, C 3) $
        emptyFGL

t1y3 = insertTripleFGL (C 1, V "y", C 3) $
        emptyFGL

t1y2 = insertTripleFGL (C 1, V "y", C 2) $
        emptyFGL

t12x_x45 = insertTripleFGL (C 1, C 2, V "x") $
           insertTripleFGL (V "x", C 4, C 5) $
           emptyFGL

t123_345 = insertTripleFGL (C 1, C 2, C 3) $
           insertTripleFGL (C 3, C 4, C 5) $
           emptyFGL
  
type TestGraph = FGLTGraph TestNode

-- testUnify :: TestGraph -> TestGraph -> Maybe (MSubst (DecompTGraph TestNode))
-- testUnify t1 t2 = unifyTGraph t1 t2

s1 :: Subst TestNode
s1 = bind ("x",C 27) idSubst