-- This module includes the definitions 
-- of the paper that contains shape expression schemas
module Paper2 where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

type URI = String

type Subject = URI
type Predicate = URI
type Object = URI

type Vs = Set Subject
type Vp = Set Predicate
type Vo = Set Object

type Triple = (Subject,Predicate,Object)

subject :: Triple -> Subject
subject (s,p,o) = s

type Graph = [Triple]

-- shape finds the triples that surround a node in a graph
shape :: Node -> Graph -> Graph
shape n = filter (\t -> subject t == n)

set :: (Ord a) => [a] -> Set a
set = Set.fromList

elems = Set.elems

union :: (Ord a) => Set a -> Set a -> Set a
union = Set.union

type Typing = Map Node (Set Label)
type Context = (Typing, Graph, Schema)

typing :: Context -> Typing
typing (t,g,s) = t

emptyTyping :: Typing
emptyTyping = Map.empty

addType :: Node -> Label -> Typing -> Typing
addType n l ts =  
  case Map.lookup n ts of
    Nothing     -> Map.insert n (Set.singleton l) ts 
    Just labels -> Map.adjust (\labels -> (Set.insert l labels)) n ts

combineTypings :: Typing -> Typing -> Typing
combineTypings ts1 ts2 = Map.union ts1 ts2

addTyping :: Node -> Label -> Context -> Context
addTyping n l (t,g,s) = (addType n l t,g,s)

graph :: Context -> Graph
graph (t,g,s) = g

findRSE :: Context -> Label -> RSE
findRSE (t,g,s) label = lookupRSE label s

type Node = Subject
type Label = String
data Var = Var Label
 deriving (Show, Eq, Ord)

type Schema = Set (Label,RSE)

lookupRSE :: Label -> Schema -> RSE
lookupRSE label = 
  snd .
  head . 
  Set.toList . 
  Set.filter (\(l,e)-> label == l)

data RSE = Fail String
         | Empty
		 | Arc Vp Vo
		 | ArcV Vp Var
		 | And RSE RSE
		 | Or RSE RSE 
		 | Star RSE
	deriving (Show,Eq,Ord)

nullable :: RSE -> Bool
nullable (Fail s) = False
nullable (Empty)  = True
nullable (Arc  _ _) = False
nullable (ArcV _ _) = False
nullable (Star e) = True
nullable (And e1 e2) = nullable e1 && nullable e2
nullable (Or e1 e2) = nullable e1 || nullable e2


deriv :: Context -> RSE -> Triple -> (RSE,Typing)
deriv ctx f@(Fail s) _ = (f,emptyTyping)
deriv ctx (Empty) t = (Fail "Deriv of empty expression",emptyTyping)
deriv ctx (Arc vp vo) (s,p,o) = 
   if Set.member p vp && Set.member o vo then (Empty,typing ctx)
   else 
    (Fail ("Does not match " ++ p ++ " with " ++ show vp ++ " and " ++ o ++ " with " ++ show vo), emptyTyping)
deriv ctx (ArcV vp (Var label)) (s,p,o) = 
   if Set.member p vp then 
    (Empty,matchSchema ctx label o)
   else 
    (Fail ("Does not match " ++ p ++ " with " ++ show vp), emptyTyping)
deriv ctx (Star e) t = 
  let (e',typing) = deriv ctx e t
  in (And e' (Star e), typing)

deriv ctx (And e1 e2) t = 
  let (e1',t1) = deriv ctx e1 t
      (e2',t2) = deriv ctx e2 t
  in (Or (And e1' e2) (And e2' e1), combineTypings t1 t2)

deriv ctx (Or e1 e2) t = 
  let (e1',t1) = deriv ctx e1 t
      (e2',t2) = deriv ctx e2 t
  in (Or e1' e2', combineTypings t1 t2)
	
match :: Context -> RSE -> Graph -> Typing
match ctx e (t:ts) = 
	let (e',typing) = deriv ctx e t
    in combineTypings typing (match ctx e' ts)
	
match ctx e [] = if nullable e then typing ctx
                   else emptyTyping

matchSchema :: Context -> Label -> Node -> Typing
matchSchema ctx label n = let rse = findRSE ctx label
                              shapeN  = shape n (graph ctx)
						  in match (addTyping n label ctx) rse shapeN
						  
schema1 :: Schema
schema1 = set [("p", Arc (set [":a"]) (set ["1"]))]

g1 :: Graph
g1 = [("n",":a","1")]

test1 = matchSchema (emptyTyping,g1,schema1) "p" "n"

arcq :: RSE
arcq = ArcV (set [":b"]) (Var "q")

--
schema2 :: Schema
schema2 = set [ ("p", And (Arc (set [":a"]) (set ["1"])) 
                          (ArcV (set [":b"]) (Var "q"))
				)
              , ("q", Arc (set [":c"]) (set ["2"]))
			  ]

g2 :: Graph
g2 = [("x",":a","1"),("x",":b","y"),("y",":c","2")]

ctx2 = (emptyTyping,g2,schema2)

test2 = matchSchema (emptyTyping,g2,schema2) "p" "x"

--
schema3 :: Schema
schema3 = set [ ("p", And (Arc (set [":a"]) (set ["1"])) 
                          (Star (ArcV (set [":b"]) (Var "q")))
				)
              , ("q", And (Arc (set [":c"]) (set ["1","2"]))
			              (Arc (set [":d"]) (set ["1","2"]))
			    )
			  ]

g3 :: Graph
g3 = [("x",":a","1"),
      ("x",":b","y"),
	  ("y",":c","1"),
	  ("y",":d","2")]

ctx3 = (emptyTyping,g3,schema3)

test3 = matchSchema (emptyTyping,g3,schema3) "p" "x"
