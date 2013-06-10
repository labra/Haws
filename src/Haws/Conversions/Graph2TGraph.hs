module Haws.Conversions.Graph2TGraph where

import Data.Maybe
import Data.Set

import Haws.TGraph
import Haws.TContext
import Haws.FGLTGraph
import Haws.Subst
import Prelude hiding (pred,succ,map,null)
import qualified Data.Graph.Inductive as FGL
import qualified Data.Map as Map

data EGraph a b = EGraph {
 eGraph :: FGLTGraph (EValue FGL.Node FGL.Node),
 eNodes :: Map.Map FGL.Node a,
 eEdges :: Map.Map b Int
} deriving Show

data EValue a b = Node a | Edge b
 deriving (Show, Eq, Ord)

graph2TGraph :: (Ord b, Show b) => FGL.Gr a b -> EGraph a b
graph2TGraph g = 
  if FGL.isEmpty g then EGraph { eGraph = gEmpty, eNodes = Map.empty, eEdges = Map.empty }
  else
   let (ctx,gn) = FGL.matchAny g
       tg = graph2TGraph gn
   in extendFromContext ctx tg
    
extendFromContext :: (Ord b, Show b) => FGL.Context a b -> EGraph a b -> EGraph a b
extendFromContext (preds,idx,node,succs) g = 
 extendSuccs succs idx $ 
 extendPreds preds idx $
 extendNode idx node g

extendNode :: FGL.Node -> a -> EGraph a b -> EGraph a b
extendNode idx node g = 
 g {
  eGraph = insertNodeFGL (Node idx) (eGraph g),
  eNodes = Map.insert idx node (eNodes g) 
 }
  
extendPreds :: (Ord b, Show b) => [(b,FGL.Node)] -> Int -> EGraph a b -> EGraph a b
extendPreds ps idx g = 
  Prelude.foldr (\p gr -> extendPred p idx gr) g ps
  
extendPred :: (Ord b, Show b) => (b,FGL.Node) -> FGL.Node -> EGraph a b -> EGraph a b
extendPred (edge,x) y g = 
 let mapEdges = eEdges g
 in case Map.lookup edge mapEdges of 
  Just p  -> g { eGraph = insertTripleFGL (Node x,Edge p,Node y) (eGraph g) }
  Nothing ->
    let p = Map.size mapEdges
    in
     g { eEdges = Map.insert edge p mapEdges ,
         eGraph = insertTripleFGL (Node x,Edge p,Node y) (eGraph g)
       }        

extendSuccs :: (Ord b, Show b) => [(b,Int)] -> Int -> EGraph a b -> EGraph a b
extendSuccs ps idx g = 
  Prelude.foldr (\p gr -> extendSucc p idx gr) g ps

extendSucc :: (Ord b) => (b,Int) -> FGL.Node -> EGraph a b -> EGraph a b
extendSucc (edge,y) x g = 
 let mapEdges = eEdges g
 in case Map.lookup edge mapEdges of 
  Just p  -> g { 
        eGraph = insertTripleFGL (Node x,Edge p,Node y) (eGraph g) 
  }
  Nothing -> 
   let p = Map.size mapEdges
   in
     g { eEdges = Map.insert edge p mapEdges,
         eGraph = insertTripleFGL (Node x,Edge p,Node y) (eGraph g)
       }        


g1 :: FGL.Gr Char String
g1 = FGL.empty

g2 = ([("p",2),("p",3)],1,'a',[("q",2)]) FGL.& (
                        ([],2,'b',[("q",3)])  FGL.& (
                        ([],3,'c',[])            FGL.& g1 ))

g3 = ([("left",2),("up",3)],1,'a',[("right",2)]) FGL.& (
                        ([],2,'b',[("down",3)])  FGL.& (
                        ([],3,'c',[])            FGL.& g1 ))

tg1 :: EGraph Char String
tg1 = EGraph { 
        eGraph = emptyFGL, 
        eNodes = Map.empty, 
        eEdges = Map.empty 
 }

tg2c :: EGraph Char String
tg2c = EGraph { 
          eGraph = insertTripleFGL (Node 1,Edge 1, Node 2) $ 
                   insertTripleFGL (Node 3,Edge 1, Node 1) $
                   insertTripleFGL (Node 2,Edge 2,Node 1) $ 
                   insertNodeFGL (Node 1) $
                   insertTripleFGL (Node 2,Edge 2,Node 3) $
                   insertNodeFGL (Node 2) $
                   insertNodeFGL (Node 3) $
                   emptyFGL,
          eNodes = Map.insert 1 'a' $
                   Map.insert 2 'b' $ 
                   Map.insert 3 'c' $
                   Map.empty,
          eEdges = Map.insert "p" 1 $
                   Map.insert "q" 2 $ 
                   Map.empty
      }


tg3c :: EGraph Char String
tg3c = EGraph { 
          eGraph = insertTripleFGL (Node 1,Edge 1, Node 2) $ 
                   insertTripleFGL (Node 3,Edge 2, Node 1) $
                   insertTripleFGL (Node 2,Edge 3,Node 1) $ 
                   insertNodeFGL (Node 1) $
                   insertTripleFGL (Node 2,Edge 4,Node 3) $
                   insertNodeFGL (Node 2) $
                   insertNodeFGL (Node 3) $
                   emptyFGL,
          eNodes = Map.insert 1 'a' $
                   Map.insert 2 'b' $ 
                   Map.insert 3 'c' $
                   Map.empty,
          eEdges = Map.insert "right" 1 $
                   Map.insert "up" 2 $ 
                   Map.insert "left" 3 $
                   Map.insert "down" 4 $
                   Map.empty
      }
      