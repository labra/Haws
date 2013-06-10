{- | 'Haws.TGraph is an abstract representation of graphs were
   nodes and edges are of the same type and where edges can be nodes -} 
module Haws.TGraph(TGraph(..)) where

import Data.Maybe
import Data.Set

import Haws.TContext
import Haws.Subst
import Prelude hiding (pred,succ,map,null)

class TGraph gr where 
   -- essential operations
   -- | An empty 'Graph'.
   gEmpty :: gr a 
 
   -- | Decompose a 'Graph' into the 'TContext' found for the given node and the
   -- remaining 'Graph'.
   -- This function is similar to 'match' in 'Data.Graph.Inductive' (we prefer the name 'decomp')
   decomp :: (Ord a, Show a) => a -> gr a -> Maybe (TContext a, gr a)

   -- | returns the nodes of the graph
   nodes :: (Ord a, Show a) => gr a -> Set a
   
   contains :: (Ord a, Show a) => a -> gr a -> Bool
   contains n g = member n (nodes g)
   
   insertTriple :: (Show a, Ord a) => (a,a,a) -> gr a -> gr a
 
   insertNode :: (Show a, Ord a) => a -> gr a -> gr a

   -- | 'decompAny' it decomposes a graph taking an arbitrary node (the head of 'nodes'). 
   --  It is similar 'matchAny' in 'Data.Graph.Inductive'
   decompAny :: (Ord a, Show a) => gr a -> Maybe (TContext a, gr a)
   decompAny g | null ns    = Nothing
               | otherwise  = decomp (head (elems ns)) g
         where ns = nodes g
                 
   -- | We could have let 'comp' as primitive and define 'insertTriple' in terms of 'comp'  
   comp :: (Ord a, Show a) => TContext a -> gr a -> gr a
   comp ctx gr =
    let n = node ctx
        gr0 = insertNode n gr
        gr1 = fold (\(p,x) g -> insertTriple (x,p,n) g) gr0 (pred ctx)
        gr2 = fold (\(p,y) g -> insertTriple (n,p,y) g) gr1 (succ ctx)
        gr3 = fold (\(x,y) g -> insertTriple (x,n,y) g) gr2 (rels ctx)
    in gr3   
   
   insertTriples :: (Show a, Ord a) => Set (a,a,a) -> gr a -> gr a
   insertTriples ts g = Data.Set.foldr (\t r -> insertTriple t r) g ts
 
   foldTGraph :: (Show a, Ord a) => b -> (TContext a -> b -> b) -> gr a -> b
   foldTGraph e h g = 
     if null (nodes g) then e
     else case decompAny g of
           Nothing -> error $ "foldTGraph: Cannot decomp graph"
           Just (ctx,g') -> h ctx (foldTGraph e h g')
   
   foldTGraphOrd :: (Ord a, Show a) => b -> (TContext a -> b -> b) -> gr a -> b
   foldTGraphOrd e h g 
      | null (nodes g) = e
      | otherwise      = case decomp (findMin (nodes g)) g of
                   Nothing -> error $ "foldTGraphOrd: Node not found in graph " 
                   Just (ctx,g') -> h ctx (foldTGraph e h g')
                   

   showFolds :: (Show a, Ord a, TGraph gr) => gr a -> String
   showFolds = foldTGraphOrd "empty\n" f
     where f :: (Show a) => TContext a -> String -> String 
           f ctx r = show ctx ++ "\n" ++ r
           
   triples :: (Show a, Ord a) => gr a -> Set (a,a,a)
   triples = foldTGraph empty (\d r -> triplesCtx d `union` r)
   
   isEmpty :: (Ord a, Show a) => gr a -> Bool
   isEmpty gr = null (nodes gr) 

   context :: (TGraph gr, Ord a, Show a) => a -> gr a -> TContext a
   context n gr = case decomp n gr of
    Nothing -> error "Cannot get context"
    Just (ctx,_) -> ctx
   
   -- simplified map 
   mapTGraph :: (Ord a, Show a, 
                  Ord b, Show b) => (a -> b) -> gr a -> gr b
   mapTGraph f = gmapTGraph (\ctx -> (mapCtx f ctx))


   -- mapTGraph2 is equivalent to mapTGraph (prove it?)
   mapTGraph2 :: (Ord a, Show a, 
                  Ord b, Show b) => (a -> b) -> gr a -> gr b
   mapTGraph2 f = foldTGraph gEmpty (\ctx g -> comp (mapCtx f ctx) g)

   -- generalized map
   gmapTGraph :: (Ord a, Show a, 
                  Ord b, Show b) => (TContext a -> TContext b) -> gr a -> gr b
   gmapTGraph f g = foldTGraph gEmpty (\ctx r -> comp (f ctx) r) g
        
   appSubstTGraph :: ( Ord a, Show a, VarCheck a, TGraph gr
                     ) => Subst a -> gr a -> gr a
   appSubstTGraph s = gmapTGraph (appSubstCtx s)
     where appSubstCtx :: (Ord a, Show a, VarCheck a) => Subst a -> TContext a -> TContext a
           appSubstCtx s = mapCtx (\x -> case varCheck x of 
                                        Nothing -> x
                                        Just v  -> appSubst s v x)

   -- | reverse the edges in a graph
   grev :: (Show a, Ord a) => gr a -> gr a
   grev = gmapTGraph swapCtx 
   
   
   -- | Generates an undirected graph from a graph (all links will be both directions)
   undir :: (Ord a, Show a) => gr a -> gr a
   undir = gmapTGraph (\ctx -> let predSucc = pred ctx `union` succ ctx
                                   revRels = map (\(x,y) -> (y,x)) (rels ctx)
                              in ctx { pred = predSucc,
                                       succ = predSucc,
                                       rels = (rels ctx) `union` revRels
                                     })
                               
   -- | returns the successors of a node in a graph                                  
   gsuc :: (Ord a, Show a) => a -> gr a -> [a]
   gsuc n g = case decomp n g of
     Nothing -> []
     Just (ctx,g') -> toList (map snd (succ ctx))               
   
   -- | returns the successors of a node in a graph                                  
   deg :: (Ord a, Show a) => a -> gr a -> Int
   deg n g = case decomp n g of
     Nothing -> 0
     Just (ctx,g') -> size (succ ctx) + size (pred ctx) 

   -- | returns the successors of a node in a graph                                  
   delete :: (Ord a, Show a) => a -> gr a -> Maybe (gr a)
   delete n g = case decomp n g of
     Nothing -> Nothing
     Just (ctx,g') -> Just g'

   isEdge :: (Ord a, Show a) => a -> gr a -> Bool
   isEdge n gr = case decomp n gr of
        Nothing -> False
        Just (ctx,_) -> not (null (rels ctx))


   isVertex :: (Ord a, Show a) => a -> gr a -> Bool
   isVertex n gr = case decomp n gr of
        Nothing -> False
        Just (ctx,_) -> not (null (succ ctx)) || not (null (pred ctx)) 
        
   gTake :: (Ord a, Show a) => Int -> gr a -> gr a
   gTake n gr 
        | n == 0 = gEmpty
        | n > 0 = case decompAny gr of
                        Nothing -> error "gTake: Cannot decomp"                
                        Just (ctx,gr') -> let grec = gTake (n - 1) gr'
                                          in comp (filterNodes (nodes grec) ctx) (gTake (n - 1) grec)
        | otherwise = error "gTake: Negative argument" 
   

-- This definition generates a decomposed graph...not needed
-- TODO: just remove the following code?
                    
data DecompTGraph a = EmptyTGraph
                    | Extend (TContext a) (DecompTGraph a)
               deriving Show

decompTGraph :: 
        ( Ord a, 
          Show a
        ) => TGraph gr => gr a -> DecompTGraph a
decompTGraph gr = 
        if isEmpty gr then EmptyTGraph
        else 
         let n = findMin (nodes gr)
         in case decomp n gr of
             Just (mctx,gr) -> Extend mctx (decompTGraph gr)
             Nothing -> error ("Not possible to decompose graph from node " ++ show n)

            