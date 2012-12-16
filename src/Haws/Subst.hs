module Haws.Subst where
import qualified Data.Map as Map
import Data.Maybe


type Subst t = Map.Map String t

idSubst :: (Ord a) => Subst a
idSubst = Map.empty

bind :: (Ord a) => (VarName,a) -> Subst a -> Subst a
bind (x,t) s = Map.insert x t s

lookupin:: (Ord a) => Subst a -> VarName -> Maybe a
lookupin s x = Map.lookup x s

appSubst :: (Ord a) => Subst a -> String -> a -> a                    
appSubst s v x = fromMaybe x (lookupin s v)                          

type VarName    = String      

class VarCheck t where
 varCheck :: t -> Maybe VarName
