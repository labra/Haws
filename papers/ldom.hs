-- Semantics of LDOM
-- Attempt to define a denotatinal semantics of LDOM
-- This paper follows the Shapes vocabulary defined by RDF Data Shapes group LDOM proposal
-- Author: Jose Emilio Labra Gayo
module LDOM where
import RDF
import Typing
import Sets
import Data.Set (Set)
import qualified Data.Set as Set

data Context = 
  Context { typing :: Typing
          , graph  ::  Graph
		  , schema :: Schema
		  }

addTyping :: Node -> Label -> Context -> Context
addTyping n l ctx = ctx { typing = addType n l (typing ctx) }

-- Find a regular shape expression in a context
-- findRSE :: Context -> Label -> Shape
-- findRSE ctx label = lookupRSE label (schema ctx)

data Var = Var Label
 deriving (Eq, Ord)
 
instance Show Var where
 show (Var v) = "?" ++ show v

data Schema = Schema (Set (Label,Shape))

instance Show Schema where
 show (Schema s) = "Schema: " ++ show s
 
lookupShape :: Label -> Schema -> Shape
lookupShape label (Schema s) = 
  snd $
  head $
  Set.toList $
  Set.filter (\(l,e)-> label == l) $
  s


  
-- Shape Expressions 

type Vo = Set Object
type Vs = Set Subject
data Unbound
data Cardinality = From Int       -- From m = m | m + 1 | ... 
                 | Range Int Int  -- Between m and n
	deriving (Eq,Ord)
	
instance Show Cardinality where
 show (From 0) = "*"
 show (From 1) = "+"
 show (From n) = "{" ++ show n ++ ",}"
 show (Range 0 1) = "?"
 show (Range m n) = "{" ++ show m ++ "," ++ show n ++ "}"

star :: Cardinality
star = From 0

plus :: Cardinality
plus = From 1

opt :: Cardinality
opt = Range 0 1

data Shape = Fail String
           | Empty
		   | Arc Predicate Vo Cardinality
		   | InvArc Predicate Vs Cardinality
		   | And Shape Shape
		   | Or Shape Shape
		   | Closed Shape
		   -- | Not RSE
		   -- | Star RSE
	deriving (Eq,Ord)
	
instance Show Shape where 
  show (Fail str)   = "fail " ++ str
  show (Empty)      = "empty"
  show (Arc p vo c)  = "arc " ++ show p ++ "-> " ++ showSet vo ++ show c
  show (InvArc p vs c)  = "invarc " ++ show p ++ "<- " ++ showSet vs ++ show c
  show (And e1 e2)  = "( " ++ show e1 ++ " , " ++ show e2 ++ " )"
  show (Or e1 e2)   = "( " ++ show e1 ++ " | " ++ show e2 ++ " )"
--  show (Not e)      = "! " ++ show e ++ " | "
--  show (Star e)     = "( " ++ show e ++ " )*"

type SingleResult = 
  ( Set Triple -- Checked triples
  , Set Triple -- Remaining triples
  , Typing     -- Resulting typing
  )
 
type Result = [SingleResult]
 
validate :: Node -> Label -> Schema -> Graph -> Result
validate n l schema graph = validateShape shape noTriples ts typing 
 where shape  = lookupShape l schema
       typing = singleTyping n l
       ts = surroundingTriples n graph
 
validateShape :: Shape -> Set Triple -> Set Triple -> Typing -> Result
validateShape (Empty) cs rs typing = [(cs, rs, typing)]

validateShape (Arc p vo (Range m n)) cs rs typing =
 validateArcRange m n p vo cs rs typing

validateShape (Arc p vo (From m)) cs rs typing =
 validateArcFrom m p vo cs rs typing
 
validateShape (And e1 e2) cs rs typing =
     undefined

	 
validateShape (Closed shape) cs rs typing =
		filter noRemaining results
  where results = validateShape shape cs rs typing 
        noRemaining :: SingleResult -> Bool
        noRemaining (cs,rs,typing) = Set.null rs

validateArcFrom :: Int -> Predicate -> Set Object -> Set Triple -> Set Triple -> Typing -> Result
validateArcFrom m _ _ _ _ _ | m < 0 = error "negative number in cardinality not allowed"
validateArcFrom 0 p vo cs rs typing = [(cs,rs,typing)]
validateArcFrom m p vo cs rs typing = 
    let splits = splitByPredicate p rs
        validated_arcs = concat (map (\(t,rs') -> validateArc p vo cs t rs' typing) splits)
    in concat (map (\(cs',rs',typing') -> validateArcFrom (m - 1) p vo cs' rs' typing') validated_arcs) 

		
validateArcRange :: Int -> Int -> Predicate -> Set Object -> Set Triple -> Set Triple -> Typing -> Result
validateArcRange 0 n p vo cs rs typing | n >= 0 = 
 if Set.size (filterMatches p vo rs) <= n then [(cs,rs,typing)]
 else []

validateArcRange m n _ _ _ _ _ | m < 0 || n < 0 = error ("Arc with incorrect range. negative values not allowed ")
validateArcRange m n _ _ _ _ _ | m > n = error ("Arc with incorrect range. m = " ++ show m ++ " > " ++ show n)

validateArcRange m n p vo cs rs typing   
 | m <= n = 
    let splits = splitByPredicate p rs
        validated_arcs = concat (map (\(t,rs') -> validateArc p vo cs t rs' typing) splits)
    in concat (map (\(cs',rs',typing') -> validateArcRange (m - 1) (n - 1) p vo cs' rs' typing') validated_arcs) 

validateArc :: Predicate -> Vo -> Set Triple -> Triple -> Set Triple -> Typing -> Result
validateArc p vo cs triple rs typing =
  if matchArc p vo triple then [(insert triple cs, rs, typing)]
  else []

matchArc :: Predicate -> Vo -> Triple -> Bool
matchArc p vo t = matchPredicate p (predicate t) &&
                  matchValue vo (object t)

matchPredicate :: Predicate -> Predicate -> Bool
matchPredicate = (==)
				  
matchValue :: Set Object -> Object -> Bool
matchValue vo o = member o vo

valueSet :: [URI] -> Vo 
valueSet = Set.fromList . map ObjectURI

splitByPredicate :: Predicate -> Set Triple -> [(Triple, Set Triple)]
splitByPredicate p ts = map (\t -> (t,Set.delete t ts)) filtered
 where filtered = Set.toList (Set.filter (\t -> predicate t == p) ts)

filterMatches :: Predicate -> Vo -> Set Triple -> Set Triple
filterMatches p vo = Set.filter (matchArc p vo)

{- | m >= 0 && m < n = 
 | m >= 0 && m == n = -}
 
{-
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
	-}

-- Tests


