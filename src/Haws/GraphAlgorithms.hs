module Haws.GraphAlgorithms where

import Haws.TGraph
import Haws.FGLTGraph
import Haws.TContext

import Data.Set
import Data.Tree 

-- | dfs = depth first search
mdfs :: (Show a, TGraph gr, Ord a) => 
           [a] -> gr a -> [a]
mdfs [] _ = []
mdfs (v:vs) g = 
 if isEmpty g then []
 else 
  case decomp v g of 
    Nothing -> mdfs vs g
    Just (ctx,g') -> v : mdfs (toList (succNodes ctx) ++ vs) g'
    
-- | bfs = breadth first search
mbfs :: (Show a, TGraph gr, Ord a) => 
           [a] -> gr a -> [a]
mbfs [] _ = []
mbfs (v:vs) g = 
 if isEmpty g then []
 else 
  case decomp v g of 
    Just (ctx,g') -> v : mbfs (vs ++ toList(succNodes ctx)) g'
    Nothing -> mbfs vs g
    
data MTree a = Br a [MTree a]
  deriving Show

postorderT :: Tree a -> [a]
postorderT t = concatMap postorderT (subForest t) ++ [(rootLabel t)]

-- depth first for TGraph's
mdf :: (Show a, Ord a, TGraph gr) => [a] -> gr a -> ([Tree a],gr a)
mdf vs g | vs == [] || isEmpty g = ([],g)
mdf (v:vs) g' = case decomp v g' of
  Just (ctx,g) -> (Node {rootLabel =v, subForest = f }: f', g2)
         where (f,  g1) = mdf (toList(succNodes ctx)) g
               (f', g2) = mdf vs g1
  Nothing -> mdf vs g'
  
mdff :: (Show a, Ord a, TGraph gr) => [a] -> gr a -> [Tree a]
mdff vs g = fst (mdf vs g)

-- | Topological sorting of TGraph's
mtopsort :: (Show a, Ord a, TGraph gr) => gr a -> [a]
mtopsort g = reverse (concatMap postorderT (mdff (toList (nodes g)) g))

-- | Strongly connected components of TGraph's
mscc :: (Show a, Ord a, TGraph gr, Show (gr a)) => gr a -> [Tree a]
mscc g = mdff (mtopsort g) (grev g)

showTrees :: Show a => [Tree a] -> String
showTrees ts = concat (Prelude.map (\t -> Data.Tree.drawTree t ++ "\n") 
    (Prelude.map (\t -> Prelude.fmap show t) ts))