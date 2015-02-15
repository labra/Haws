------------------------------------
-- Shape Typings
module Typing(Typing(..),
	emptyTyping,
	addType,
	singleTyping,
	combineTypings,
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
 deriving Eq

instance Show Typing where
 show (Typing t) = concat (intersperse "," (map (\(n,ls) -> show n ++ "->" ++ showLabels ls) (Map.toList t)))
	where showLabels ls = showSet ls
				   
emptyTyping :: Typing
emptyTyping = Typing (Map.empty)


addType :: Node -> Label -> Typing -> Typing
addType n l (Typing ts) =  
  case Map.lookup n ts of
    Nothing     -> Typing (Map.insert n (Set.singleton l) ts) 
    Just labels -> Typing (Map.adjust (\labels -> (Set.insert l labels)) n ts)

singleTyping :: Node -> Label -> Typing
singleTyping n l = addType n l emptyTyping
	
combineTypings :: Typing -> Typing -> Typing
combineTypings (Typing ts1) (Typing ts2)= Typing (Map.union ts1 ts2)

--
