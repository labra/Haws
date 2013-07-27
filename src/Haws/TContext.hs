{- | Definitions around contexts of nodes in 'MGraph' graphs -}
module Haws.TContext where
import Data.Set

import Prelude hiding(succ,pred,filter,map)

data TContext a = Ctx {
        node :: a, 
        pred :: Set (a,a),
        succ :: Set (a,a),
        rels :: Set (a,a) 
} deriving (Show,Eq,Ord)

triplesCtx :: Ord a => TContext a -> Set(a,a,a)
triplesCtx ctx = 
        fold (\(p,y) r -> insert (node ctx,p,y) r) empty (succ ctx) `union`
        fold (\(p,x) r -> insert (x,p,node ctx) r) empty (pred ctx) `union`
        fold (\(x,y) r -> insert (x,node ctx,y) r) empty (rels ctx)       
 

deleteNodeCtx :: (Show a, Ord a) => a -> TContext a -> TContext a
deleteNodeCtx n ctx = 
        if ((node ctx) == n) 
        then error ("Trying to delete node " ++ show n ++ " from its own context")
        else Ctx { node = node ctx, 
                   succ = filter(\(p,y) -> p /= n && y /= n) (succ ctx),
                   pred = filter(\(p,x) -> p /= n && x /= n) (pred ctx),
                   rels = filter(\(x,y) -> x /= n && y /= n) (rels ctx)
                 }

mergeSucc :: Ord a => (a,a) -> TContext a -> TContext a
mergeSucc (p,y) ctx = ctx { succ = insert (p,y) (succ ctx) } 
         
mergePred :: Ord a => (a,a) -> TContext a -> TContext a
mergePred (p,x) ctx = ctx { pred = insert (p,x) (pred ctx ) }

mergeRels :: Ord a => (a,a) -> TContext a -> TContext a
mergeRels (x,y) ctx = ctx { rels = insert (x,y) (rels ctx) }

mapPairs :: (Ord a, Ord b) => (a -> b) -> Set(a,a) -> Set(b,b)
mapPairs f = map (\(x,y) -> (f x,f y))
                   
succNodes :: Ord a => TContext a -> Set a
succNodes ctx = map (snd) (succ ctx)

mapCtx :: (Ord a, Ord b) => (a -> b) -> TContext a -> TContext b
mapCtx f ctx = ctx { node = f (node ctx), 
                     succ = mapPairs f (succ ctx),
                     pred = mapPairs f (pred ctx),
                     rels = mapPairs f (rels ctx) }
                     

swapCtx :: Ord a => TContext a -> TContext a
swapCtx ctx = ctx { pred = succ ctx, succ = pred ctx, rels = map (\(x,y) -> (y,x)) (rels ctx) }

filterNodes :: Ord a => Set a -> TContext a -> TContext a
filterNodes ns ctx = ctx { succ = filterNodesPairs ns (succ ctx),
                           pred = filterNodesPairs ns (pred ctx),
                           rels = filterNodesPairs ns (rels ctx)}
                           
filterNodesPairs :: Ord a => Set a -> Set (a,a) -> Set (a,a)
filterNodesPairs ns ps = filter (\(x,y) -> not (member x ns || member y ns)) ps