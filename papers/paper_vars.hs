-- Example code for a paper about ShEx
-- This code is a simplified self-contained implementation of Regular Shape Expressions
-- Author: Jose Emilio Labra Gayo
module Paper_Vars where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Test.Framework 
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Node)

-- Preliminary definitions
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

-- Shape Typings
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

-- Find a regular shape expression in a context
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

  
-- Regular Shape Expressions with Vars

data RSE = Fail String
         | Empty
		 | Arc Vp Vo
		 | ArcV Vp Var   -- Arc with a variable
		 | And RSE RSE
		 | Or RSE RSE 
		 | Star RSE
	deriving (Show,Eq,Ord)

-- nullable
nullable :: RSE -> Bool
nullable (Fail s) = False
nullable (Empty)  = True
nullable (Arc  _ _) = False
nullable (ArcV _ _) = False
nullable (Star e) = True
nullable (And e1 e2) = nullable e1 && nullable e2
nullable (Or e1 e2) = nullable e1 || nullable e2

-- derivative taking into account the context
-- it returns the derivative and the new typing
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

-- Tests
						  
schema1 :: Schema
schema1 = set [("p", Arc (set [":a"]) (set ["1"]))]

g1 :: Graph
g1 = [("x",":a","1")]

arcq :: RSE
arcq = ArcV (set [":b"]) (Var "q")

result1 = addType "x" "p" $ emptyTyping

test1 = matchSchema (emptyTyping,g1,schema1) "p" "x" @?= result1

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

result2 = addType "y" "q" $ addType "x" "p" $ emptyTyping

test2 = matchSchema (emptyTyping,g3,schema3) "p" "x" @?= result2

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
g3 = [("x",":a","1"),("x",":b","y"),("y",":c","1"),("y",":d","2")]

ctx3 = (emptyTyping,g3,schema3)

result3 = addType "y" "q" $ addType "x" "p" $ emptyTyping

test3 = matchSchema (emptyTyping,g3,schema3) "p" "x" @?= result3

main = defaultMain tests

tests = [
        testGroup "Matching schemas" [
			  testCase "RSE 1 with zero nodes" test1
			, testCase "RSE 2 with one nodes" test2
            , testCase "RSE 3 with two nodes" test3 
            ]
    ]

