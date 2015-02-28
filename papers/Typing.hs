------------------------------------
-- Shape Typings
module Typing(Typing(..),
	emptyTyping,
	addType,addTypes,
	singleTyping,
	combineTypings,
	contains,
	Label) where
import Data.Either(Either(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List(intersperse)

import RDF
import Sets

type Label = URI

data Typing = Typing (Map Node (Set Label))
 
instance Eq Typing where
 (Typing t1) == (Typing t2) = Map.toAscList t1 == Map.toAscList t2

instance Show Typing where
 show (Typing t) = concat (intersperse "," (map (\(n,ls) -> show n ++ "->" ++ showLabels ls) (Map.toList t)))
	where showLabels ls = showSet ls
				   
emptyTyping :: Typing
emptyTyping = Typing (Map.empty)

addTypes :: Node -> [Label] -> Typing -> Typing
addTypes n ls typ = foldr (\lbl t -> addType n lbl t) typ ls

addType :: Node -> Label -> Typing -> Typing
addType n l (Typing ts) =  
  case Map.lookup n ts of
    Nothing     -> Typing (Map.insert n (Set.singleton l) ts) 
    Just labels -> Typing (Map.adjust (\labels -> (Set.insert l labels)) n ts)

singleTyping :: Node -> Label -> Typing
singleTyping n l = addType n l emptyTyping
	
combineTypings :: Typing -> Typing -> Typing
combineTypings (Typing ts1) (Typing ts2)= 
  Typing (Map.unionWith (\s1 s2 -> s1 `Set.union` s2) ts1 ts2)

contains :: Typing -> Node -> Label -> Bool
contains (Typing t) n label = 
 case Map.lookup n t of
  Nothing -> False
  Just lbls -> member label lbls
  

--
