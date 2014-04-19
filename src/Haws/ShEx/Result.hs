-- Results as backtracking monad
module Haws.ShEx.Result where
import Haws.Monads.BackMonad
import Data.Set (Set)
import qualified Data.Set as Set

data Result a = R {rs::[a]}
 deriving (Show,Eq)

instance Monad Result where 
 return x = R {rs = [x]}
 (>>=) = bindResult

returnResult :: a -> Result a
returnResult x = R {rs = [x]}

bindResult :: Result a -> (a -> Result b) -> Result b
bindResult ra f = 
 let rbs = map f (rs ra)
 in concatResult rbs

instance BackMonad Result where 
 orelse = orelseResult
 failure = R { rs = [] }
 
 
concatResult :: [Result b] -> Result b
concatResult ls = R { rs = foldr (\x r -> rs x ++ r) [] ls }

orelseResult :: Result b -> Result b -> Result b
orelseResult r1 r2 = R { rs = rs r1 ++ rs r2 }

parts :: (Ord a) => Set a -> Result (Set a,Set a)
parts s = R { rs = map setFromList2 (pSet (Set.toList s)) }
 where setFromList2 :: (Ord a) => ([a],[a]) -> (Set a, Set a)
       setFromList2 (xs,ys) = (Set.fromList xs, Set.fromList ys)

pSet :: [a] -> [([a],[a])]
pSet [] = [([],[])]
pSet (x:xs) = mapx first ++ mapx second where
 mapx which = map (which (x:)) $ pSet xs
 first f  (x,y) = (f x, y)
 second f (x,y) = (x, f y)
