module Haws.FunTGraph where 
import Data.Maybe
import Data.Set

import Haws.TGraph
import Haws.TContext
import Prelude hiding (succ,pred)

-- A basic generic graph is a list of nodes and a function from nodes to contexts 
data FunTGraph a = FunTGraph (a -> Maybe (TContext a, FunTGraph a)) (Set a)

instance Show a => Show (FunTGraph a) where 
 show = showFunTGraph  

instance TGraph FunTGraph where 
  gEmpty = emptyFunTGraph
  decomp = decompFun 
  nodes = nodesFun
  insertTriple = insertTripleFunTGraph
  insertNode = insertNodeFun

showFunTGraph :: (Show a) => FunTGraph a -> String
showFunTGraph (FunTGraph f ns) = 
  fold (\n r -> show (fst (fromJust (f n))) ++ "\n" ++ r) [] ns

emptyFunTGraph :: FunTGraph a
emptyFunTGraph = FunTGraph (\_ -> Nothing) empty

decompFun :: a -> FunTGraph a -> Maybe (TContext a, FunTGraph a)
decompFun n (FunTGraph f _) = f n 

nodesFun :: FunTGraph a -> Set a
nodesFun (FunTGraph _ ns) = ns


insertNodeFun :: (Show a, Ord a) => a -> FunTGraph a -> FunTGraph a
insertNodeFun e g@(FunTGraph f ns) = 
        if contains e g then g
        else FunTGraph (\n -> 
                if n == e then 
                  Just (Ctx {node = n,pred = empty,succ = empty, rels = empty},
                        remove n g)
                else f n) 
        (insert e ns)


insertEdgeCtx :: (Show a, Ord a) => (a,a,a) -> FunTGraph a -> a -> Maybe (TContext a, FunTGraph a)
insertEdgeCtx (x,p,y) g@(FunTGraph f _) n 
  | n == x = Just (mergeSucc (p,y) (context x g), remove x g)
  | n == p = Just (mergeRels (x,y) (context p g), remove p g)
  | n == y = Just (mergePred (x,p) (context y g), remove y g)
  | otherwise = f n 

remove :: (Show a, Ord a) => a -> FunTGraph a -> FunTGraph a
remove e (FunTGraph f xs) = 
        FunTGraph (\n -> if n == e then Nothing
                     else case f n of 
                        Just (ctx,g) -> Just (deleteNodeCtx e ctx, remove e g)
                        Nothing -> Nothing) 
              (Data.Set.delete e xs)

insertTripleFunTGraph :: (Show a, Ord a) => (a,a,a) -> FunTGraph a -> FunTGraph a
insertTripleFunTGraph (x,p,y) g = 
        let g1 = insertNodeFun x g
            g2 = insertNodeFun p g1
            g3 = insertNodeFun y g2
        in insertEdge (x,p,y) g3

insertEdge :: (Show a, Ord a) => (a,a,a) -> FunTGraph a -> FunTGraph a
insertEdge (x,p,y) g@(FunTGraph _ ns) =
        FunTGraph (insertEdgeCtx (x,p,y) g) ns


-- ex 1 g(1,2,3)
ex1 :: FunTGraph Int
ex1 = FunTGraph (\n -> case n of
  1 -> Just (Ctx {node = 1,pred = fromList [(2,3)],succ = empty,rels = empty},gEmpty)
  2 -> Just (Ctx {node = 2,pred = empty,succ = empty,rels = fromList [(1,3)]},gEmpty)
  3 -> Just (Ctx {node = 3,pred = empty,succ = fromList [(1,2)],rels = empty},gEmpty)
  _ -> Nothing)
  (fromList [1,2,3])

ex2 :: FunTGraph Int
ex2 = insertNodeFun 4 ex1
