-- This module includes the definitions 
-- of the paper that contains shape expression schemas
module Paper2 where

import Data.Set (Set)
import qualified Data.Set as Set

type URI = String

type Subject = URI
type Predicate = URI
type Object = URI

type Vs = Set Subject
type Vp = Set Predicate
type Vo = Set Object

type Triple = (Subject,Predicate,Object)
type Graph = [Triple]


set :: (Ord a) => [a] -> Set a
set = Set.fromList

elems = Set.elems

union :: (Ord a) => Set a -> Set a -> Set a
union = Set.union

type Typing = [(Node, Label)]
type Context = Typing

type Node = Subject
type Label = String
data Var = Var Label
 deriving (Show, Eq, Ord)

type Schema = Set (Label,RSE)

data RSE = Fail String
         | Empty
		 | Arc Vp Vo
		 | ArcV Vp Var
		 | And RSE RSE
		 | Or RSE RSE 
		 | Star RSE
	deriving (Show,Eq)

nullable :: RSE -> Bool
nullable (Fail s) = False
nullable (Empty)  = True
nullable (Arc  _ _) = False
nullable (ArcV _ _) = False
nullable (Star e) = True
nullable (And e1 e2) = nullable e1 && nullable e2
nullable (Or e1 e2) = nullable e1 || nullable e2


deriv :: Context -> RSE -> Triple -> (RSE,Typing)
deriv ctx f@(Fail s) _ = f
deriv ctx (Empty) t = Fail "Deriv of empty expression"
deriv ctx (Arc vp vo) (s,p,o) = 
   if Set.member p vp && Set.member o vo then Empty
   else Fail ("Does not match " ++ p ++ " with " ++ show vp ++ " and " ++ o ++ " with " ++ show vo)
deriv ctx (Star e) t = And (deriv ctx e t) (Star e)
deriv ctx (And e1 e2) t = Or (And (deriv ctx e1 t) e2)
                         (And (deriv ctx e2 t) e2)
deriv ctx (Or e1 e2) t = Or (deriv ctx e1 t) (deriv ctx e2 t)
