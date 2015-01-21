-- Example code for a paper about ShEx
-- This code is a simplified self-contained implementation of Regular Shape Expressions
-- Author: Jose Emilio Labra Gayo
module Paper where

import Data.Set (Set)
import qualified Data.Set as Set

-- Preliminary definitions

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
type Graph = [Triple]

-- Extension of a RSE is a set of set of triples
-- Notice that the definition Star does not stop
-- This definition has been included only for type checking purposes
extend :: RSE -> Subject -> Set (Set Triple)
extend (Fail str) _ = error ("extend of fail: " ++ str)
extend (Empty) _ = set []
extend (Arc vp vo) s = set [set [(s,p,o)]| p <- elems vp, o <- elems vo]
extend (And e1 e2) s = let se1 = extend e1 s
                           se2 = extend e2 s
					   in set [ union t1 t2 | t1 <- elems se1, t2 <- elems se2]
extend (Or e1 e2) s = union (extend e1 s) (extend e2 s)
extend (Star e) s = union (set []) (extend (And e (Star e)) s)

-- Derivatives of Regular Shape Expressions

-- Definition of nullable		   
nullable :: RSE -> Bool
nullable (Fail s) = False
nullable (Empty)  = True
nullable (Arc _ _) = False
nullable (Star e) = True
nullable (And e1 e2) = nullable e1 && nullable e2
nullable (Or e1 e2) = nullable e1 || nullable e2

-- Definition of derivative
deriv :: RSE -> Triple -> RSE
deriv f@(Fail s) _ = f
deriv (Empty) t = Fail "Deriv of empty expression"
deriv (Arc vp vo) (s,p,o) = 
   if Set.member p vp && Set.member o vo then Empty
   else Fail ("Does not match " ++ p ++ " with " ++ show vp ++ " and " ++ o ++ " with " ++ show vo)
deriv (Star e) t = And (deriv e t) (Star e)
deriv (And e1 e2) t = Or (And (deriv e1 t) e2)
                         (And (deriv e2 t) e1)
deriv (Or e1 e2) t = Or (deriv e1 t) (deriv e2 t)
                       

-- Matching Regular Shape Expressions
match :: RSE -> Graph -> Bool
match e (t:ts) = match (deriv e t) ts
match e []     = nullable e

-- Some tests
-- Some Regular Shape Expressions
rse0 :: RSE
rse0 = Arc (set [":a","b"]) (set ["1","2"])

rse1 :: RSE
rse1 = And (Arc (set [":a","b"]) (set ["1","2"]))
           (Arc (set [":c","d"]) (set ["3","4"]))

rse2 :: RSE 
rse2 = And (Arc (set [":a"]) (set ["1"]))
           (Star (Arc (set [":b"]) (set ["1","2"])))

g01 = [("n",":a","1")]
g02 = [("n",":a","xx")]
g1 = [("n",":a","1"),("n",":d","3")]
g2 = [("n",":a","1"),("n",":b","1"),("n",":b","2")]

test01 = match rse0 g01 
test02 = not (match rse0 g02)
test11 = not (match rse1 g1)
test2 = match rse2 g2 

tests = all (==True) [ test01, test02, test11, test2 ]

