-- Example code for a paper about ShEx
-- This code is a simplified self-contained implementation of Regular Shape Expressions
-- In this code we add negations and open shapes
-- Author: Jose Emilio Labra Gayo
module Paper_Vars where

import Data.List(intersperse)

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

predicate :: Triple -> Predicate
predicate (s,p,o) = p

object :: Triple -> Object
object (s,p,o) = o

data Graph = Graph [Triple]

-- shape finds the triples that surround a node in a graph
shape :: Node -> Graph -> Graph
shape n (Graph ts) = 
	Graph (filter (\t -> subject t == n) ts)

set :: (Ord a) => [a] -> Set a
set = Set.fromList

elems = Set.elems

union :: (Ord a) => Set a -> Set a -> Set a
union = Set.union

showSet :: Show a => Set a -> String
showSet s = "{" ++ concat (intersperse "," (Set.toList (Set.map show s))) ++ "}"

-- Shape Typings
data Typing = Typing (Map Node (Set Label))
 deriving Eq

instance Show Typing where
 show (Typing t) = concat (intersperse "," (map (\(n,ls) -> n ++ "->" ++ showLabels ls) (Map.toList t)))
	where showLabels ls = showSet ls
				   
emptyTyping :: Typing
emptyTyping = Typing (Map.empty)

addType :: Node -> Label -> Typing -> Typing
addType n l (Typing ts) =  
  case Map.lookup n ts of
    Nothing     -> Typing (Map.insert n (Set.singleton l) ts) 
    Just labels -> Typing (Map.adjust (\labels -> (Set.insert l labels)) n ts)

combineTypings :: Typing -> Typing -> Typing
combineTypings (Typing ts1) (Typing ts2)= Typing (Map.union ts1 ts2)

data Context = 
  Context { typing :: Typing
          , graph  ::  Graph
		  , schema :: Schema
		  }

addTyping :: Node -> Label -> Context -> Context
addTyping n l ctx = ctx { typing = addType n l (typing ctx) }

-- Find a regular shape expression in a context
findRSE :: Context -> Label -> RSE
findRSE ctx label = lookupRSE label (schema ctx)

type Node = Subject
type Label = String
data Var = Var Label
 deriving (Eq, Ord)
 
instance Show Var where
 show (Var v) = "?" ++ v

data Schema = Schema (Set (Label,RSE))

instance Show Schema where
 show (Schema s) = "Schema: " ++ show s
 
lookupRSE :: Label -> Schema -> RSE
lookupRSE label (Schema s) = 
  snd $
  head $
  Set.toList $
  Set.filter (\(l,e)-> label == l) $
  s

  
-- Regular Shape Expressions with Vars

data RSE = Fail String
         | Empty
		 | Arc Vp Vo
		 | ArcV Vp Var   -- Arc with a variable
		 | And RSE RSE
		 | Or RSE RSE 
		 | Not RSE
		 | Star RSE
	deriving (Eq,Ord)
	
instance Show RSE where 
  show (Fail str)   = "fail " ++ str
  show (Empty)      = "empty"
  show (Arc vp vo)  = "arc " ++ showSet vp ++ "-> " ++ showSet vo 
  show (ArcV vp vo) = "arcV " ++ showSet vp ++ "-> " ++ show vo
  show (And e1 e2)  = "( " ++ show e1 ++ " , " ++ show e2 ++ " )"
  show (Or e1 e2)   = "( " ++ show e1 ++ " | " ++ show e2 ++ " )"
  show (Not e)      = "! " ++ show e ++ " | "
  show (Star e)     = "( " ++ show e ++ " )*"

-- nullable
nullable :: RSE -> Bool
nullable (Fail s) = False
nullable (Empty)  = True
nullable (Arc  _ _) = False
nullable (ArcV _ _) = False
nullable (Star e) = True
nullable (Not e) = not (nullable e)
nullable (And e1 e2) = nullable e1 && nullable e2
nullable (Or e1 e2) = nullable e1 || nullable e2

-- derivative taking into account the context
-- it returns the derivative, 
-- the new typing, 
-- the consumed triples and the remainder
deriv :: Context -> RSE -> Triple -> (RSE,Typing)
deriv ctx f@(Fail s) _ = (f,emptyTyping)
deriv ctx (Empty) t = (Fail "Deriv of empty expression",emptyTyping)
deriv ctx (Arc vp vo) t = 
   if Set.member (predicate t) vp && Set.member (object t) vo then (Empty,typing ctx)
   else 
    (Fail ("Does not match " ++ predicate t ++ " with " ++ show vp ++ " and " ++ object t ++ " with " ++ show vo), 
	       emptyTyping)
		   
deriv ctx (ArcV vp (Var label)) t = 
   if Set.member (predicate t) vp then 
    (Empty,matchSchema ctx label (object t))
   else 
    (Fail ("Does not match " ++ predicate t ++ " with " ++ show vp), emptyTyping)
	
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

foldGraph :: (Triple -> a -> a) -> a -> Graph -> a
foldGraph f e (Graph ts) = foldr f e ts

match :: Context -> RSE -> Graph -> Typing
match ctx e (Graph (t:ts)) = 
	let (e',typing) = deriv ctx e t
    in combineTypings typing (match ctx e' (Graph ts))
match ctx e (Graph []) = 
	if nullable e then typing ctx
                  else emptyTyping

matchSchema :: Context -> Label -> Node -> Typing
matchSchema ctx label n = let rse = findRSE ctx label
                              shapeN  = shape n (graph ctx)
						  in match (addTyping n label ctx) rse shapeN
				  
matchStep :: Context -> RSE -> Graph -> [(RSE,Typing)]
matchStep ctx e (Graph (t:ts)) = 
    let (e',typing) = deriv ctx e t
        next        = matchStep ctx e' (Graph ts)
        nextTyping  = snd (head next)
    in (e, combineTypings typing nextTyping) : next
matchStep ctx e (Graph []) = 
	if nullable e then [(e,typing ctx)]
                  else [(Fail "Non nullable", emptyTyping)]

matchSchemaStep :: Context -> Label -> Node -> [(RSE,Typing)]
matchSchemaStep ctx label n = 
    let rse = findRSE ctx label
        shapeN  = shape n (graph ctx)
	in matchStep (addTyping n label ctx) rse shapeN

showSteps :: Show a => [a] -> IO ()
showSteps steps = putStrLn $ foldr (\l r -> show l ++ "\n" ++ r) "" steps
	
-- Tests

arc1 = 	Arc (set [":a"]) (set ["1"])
					  
schema1 :: Schema
schema1 = Schema (set [("p", arc1)])


g1 :: Graph
g1 = Graph [("x",":a","1")]

result1 = addType "x" "p" $ emptyTyping

ctx1 = Context { typing = emptyTyping, graph = g1, schema = schema1 }

test_findRSE1 = findRSE ctx1 "p" @?= arc1


test1 = matchSchema ctx1 "p" "x" @?= result1

test1step = showSteps $ matchSchemaStep ctx1 "p" "x"


--
schema2 :: Schema
schema2 = Schema (set [ ("p", And (Arc (set [":a"]) (set ["1"])) 
                                  (ArcV (set [":b"]) (Var "q"))
				        )
                      , ("q", Arc (set [":c"]) (set ["2"]))
			          ])

g2 :: Graph
g2 = Graph [("x",":a","1"),("x",":b","y"),("y",":c","2")]

ctx2 = Context { typing = emptyTyping, graph = g2, schema = schema2 }

result2 = addType "y" "q" $ 
          addType "x" "p" $ emptyTyping

		  
test2 = matchSchema ctx2 "p" "x" @?= result2


test2step = showSteps $ matchSchemaStep ctx2 "p" "x" 

g21 :: Graph
g21 = Graph [("x",":a","1"),("x",":b","2")]

ctx21 = Context { typing = emptyTyping, graph = g21, schema = schema2 }
result21 = addType "y" "q" $ 
           addType "x" "p" $ emptyTyping

test21 = matchSchema ctx21 "p" "x" @?= result21
test21step = matchSchemaStep ctx21 "p" "x" 

--
schema3 :: Schema
schema3 = Schema (set [ ("p", And (Arc (set [":a"]) (set ["1"])) 
                             (Star (ArcV (set [":b"]) (Var "q")))
				)
              , ("q", And (Arc (set [":c"]) (set ["1","2"]))
			              (Arc (set [":d"]) (set ["1","2"]))
			    )
			  ])

g3 :: Graph
g3 = Graph [("x",":a","1"),("x",":b","y"),("y",":c","1"),("y",":d","2")]

ctx3 = Context { typing = emptyTyping, graph = g2, schema = schema2 }

result3 = addType "y" "q" $ addType "x" "p" $ emptyTyping

test3 = matchSchema ctx3 "p" "x" @?= result3
test3step = matchSchemaStep ctx3 "p" "x" 

main = defaultMain tests

tests = [
        testGroup "Utilities" [
		 testCase "findRSE 1" test_findRSE1
		],
        testGroup "Matching schemas" [
			  testCase "RSE 1 with zero nodes" test1
			, testCase "RSE 2 with one nodes" test2
			, testCase "RSE 21 with one nodes" test21
            , testCase "RSE 3 with two nodes" test3 
            ]
    ]

