module Paper where

import Data.Set (Set)
import qualified Data.Set as Set

type URI = String

type Subject = URI
type Predicate = URI
type Object = URI

type Vs = Set Subject
type Vp = Set Predicate
type Vo = Set Object

set :: (Ord a) => [a] -> Set a
set = Set.fromList

elems = Set.elems

union :: (Ord a) => Set a -> Set a -> Set a
union = Set.union

-- RSE = Regular Shape Expressions
data RSE = Fail String
         | Empty
		 | Arc Vp Vo
		 | And RSE RSE
		 | Or RSE RSE 
		 | Star RSE
	deriving (Show,Eq)
	
type Triple = (Subject,Predicate,Object)
	
extend :: RSE -> Subject -> Set (Set Triple)
extend (Fail str) _ = error ("extend of fail: " ++ str)
extend (Empty) _ = set []
extend (Arc vp vo) s = set [set [(s,p,o)]| p <- elems vp, o <- elems vo]
extend (And e1 e2) s = let se1 = extend e1 s
                           se2 = extend e2 s
					   in set [ union t1 t2 | t1 <- elems se1, t2 <- elems se2]
extend (Or e1 e2) s = union (extend e1 s) (extend e2 s)
extend (Star e) s = union (set []) (extend (And e (Star e)) s)

rse0 :: RSE
rse0 = Arc (set [":a","b"]) (set ["1","2"])

rse1 :: RSE
rse1 = And (Arc (set [":a","b"]) (set ["1","2"]))
           (Arc (set [":c","d"]) (set ["3","4"]))

rse2 :: RSE 
rse2 = And (Arc (set [":a"]) (set ["1"]))
           (Star (Arc (set [":b"]) (set ["1","2"])))
		   
