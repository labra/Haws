-- Semantics of LDOM
-- Denotatinal semantics of LDOM
-- This paper follows the Shapes vocabulary defined by RDF Data Shapes group LDOM proposal
-- Author: Jose Emilio Labra Gayo
module LDOM where
import RDF
import Typing
import Sets
import Data.Set (Set)
import qualified Data.Set as Set

data Schema = Schema (Set (Label,Shape))

instance Show Schema where
 show (Schema s) = "Schema: " ++ showSet s
 
lookupShape :: Label -> Schema -> Shape
lookupShape label (Schema s) = 
  snd $
  head $
  Set.toList $
  Set.filter (\(l,e)-> label == l) $
  s

emptySchema = Schema Set.empty
  
-- Shape Expressions 

data Value = ValueSet (Set Object)
           | ValueType URI
		   | ValueRef Label
	deriving (Eq,Ord)

instance Show Value where
 show (ValueSet s) = "(" ++ showSet s ++ ")"
 show (ValueType uri) = show uri
 show (ValueRef lbl) = "@" ++ show lbl
				 
			  
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
		   | Arc Predicate Value Cardinality
		   | InvArc Predicate Value Cardinality
		   | And Shape Shape
		   | Or Shape Shape
		   | Xor Shape Shape
		   | Not Shape
		   | Closed Shape
	deriving (Eq,Ord)
	
instance Show Shape where 
  show (Fail str)   = "fail " ++ str
  show (Empty)      = "empty"
  show (Arc p v c)  = "arc " ++ show p ++ "-> " ++ show v ++ show c
  show (InvArc p v c)  = "invarc " ++ show p ++ "<- " ++ show v ++ show c
  show (And e1 e2)  = "( " ++ show e1 ++ " , " ++ show e2 ++ " )"
  show (Or e1 e2)   = "( " ++ show e1 ++ " | " ++ show e2 ++ " )"
  show (Xor e1 e2)   = "( " ++ show e1 ++ " |xor| " ++ show e2 ++ " )"
  show (Not e)      = "(! " ++ show e ++ ")"
  show (Closed e)      = "[ " ++ show e ++ "]"

data ValidationState = ValidationState { 
    checked   :: Set Triple -- Checked triples
  , remaining :: Set Triple -- Remaining triples
  , typing    :: Typing     -- Resulting typing
  }
 deriving (Eq)

instance Show ValidationState where 
 show s = "\nchecked: " ++ showSet (checked s) ++ "\n" ++
          "remaining: " ++ showSet (remaining s) ++ "\n" ++
		  "typing: " ++ show (typing s)

state :: (Typing, Set Triple, Set Triple) -> ValidationState
state (t,cs,rs) = ValidationState { typing = t, checked = cs, remaining = rs }
		  
type Result = [ValidationState]

data Context = Context { 
  graph  :: Graph
, schema :: Schema 
, currentTyping :: Typing
}


addTyping :: Node -> Label -> Context -> Context
addTyping n lbl ctx = ctx { currentTyping = addType n lbl (currentTyping ctx) }

validate :: Node -> Label -> Context -> Result
validate n lbl ctx = matchNode lbl n ctx

matchNode :: Label -> Node -> Context -> Result
matchNode lbl n ctx = matchShape shape ts (addTyping n lbl ctx)
 where shape  = lookupShape lbl (schema ctx)
       ts = surroundingTriples n (graph ctx)
	   
matchShape :: Shape -> Set Triple -> Context -> Result
matchShape Empty ts ctx = [state (currentTyping ctx, noTriples, ts)]

matchShape (Arc p v (From m)) ts ctx =
 matchArcFrom m p v ts ctx


matchShape (Arc p v (Range m n)) ts ctx =
 matchArcRange m n p v ts ctx

 
matchShape (And e1 e2) ts ctx =
 do {
   state1 <- matchShape e1 ts ctx
 ; state2 <- matchShape e2 (remaining state1) ctx
 ; return (state2 { typing = combineTypings (typing state1) (typing state2)
                  , checked = checked state1 `Set.union` checked state2
                  }) 
 }

matchShape (Or e1 e2) ts ctx =
      matchShape e1 ts ctx ++
	  matchShape e2 ts ctx

matchShape (Xor e1 e2) ts ctx =
      matchShape (Or (And e1 (Not e2)) (And e2 (Not e1))) ts ctx

matchShape (Not e) ts ctx = 
       if null (matchShape e ts ctx) 
	   then [state (currentTyping ctx, ts, noTriples) ]
	   else []
	  
matchShape (Closed shape) ts ctx = filter noRemaining results
  where results = matchShape shape ts ctx
        noRemaining :: ValidationState -> Bool
        noRemaining s = Set.null (remaining s)

combine :: Typing -> Triple -> ValidationState -> ValidationState
combine t triple s = 
 s { typing = combineTypings t (typing s)
   , checked = insert triple (checked s)
   }

-- validateArcFrom
matchArcFrom :: Int -> Predicate -> Value -> Set Triple -> Context -> Result
matchArcFrom m _ _ _ _ | m < 0 = error "negative number in cardinality not allowed"
matchArcFrom 0 p v ts ctx = [state (currentTyping ctx,noTriples,ts)]
matchArcFrom m p v ts ctx | m >= 1 = 
 do {
   (triple,rs) <- decompByPredicate p ts
 ; typing <- matchArc p v triple ctx
 ; state <- matchArcFrom (m - 1) p v rs ctx
 ; return (combine typing triple state)
 }

 
-- validateArcRange 
matchArcRange :: Int -> Int -> Predicate -> Value -> Set Triple -> Context -> Result
 
matchArcRange 0 n p v ts ctx | n >= 0 = 
 if noMatchArc p v ts ctx 
 then [state (currentTyping ctx,noTriples,ts)]
 else
  do {
    (triple,rs) <- decompByPredicate p ts
  ; typing <- matchArc p v triple ctx
  ; state <- matchArcRange 0 (n - 1) p v rs ctx
  ; return (combine typing triple state)
  }

matchArcRange m n _ _ _ _ | m < 0 || n < 0 = []
matchArcRange m n _ _ _ _ | m > n = error ("Arc with incorrect range. m = " ++ show m ++ " > " ++ show n)

matchArcRange m n p v ts ctx   
 | m <= n && m > 0 = 
 do {
   (triple,rs) <- decompByPredicate p ts
 ; typing <- matchArc p v triple ctx
 ; state  <- matchArcRange (m - 1) (n - 1) p v rs ctx
 ; return (combine typing triple state)
 }

noMatchArc :: Predicate -> Value -> Set Triple -> Context -> Bool
noMatchArc p v ts ctx = matchArcAny p v ts ctx == []
	
matchArcAny :: Predicate -> Value -> Set Triple -> Context -> [Typing]
matchArcAny p v ts ctx = if Set.null ts then []
 else let (t,ts') = divide ts
      in matchArc p v t ctx ++ 
	     matchArcAny p v ts' ctx
 
matchArc :: Predicate -> Value -> Triple -> Context -> [Typing]
matchArc p v t ctx = 
  if matchPredicate p (predicate t) 
  then matchValue v (object t) ctx
  else []
  
matchPredicate :: Predicate -> Predicate -> Bool
matchPredicate = (==)
				  
matchValue :: Value -> Object -> Context -> [Typing]
matchValue (ValueSet s) o ctx = 
 if member o s then [emptyTyping]
 else []

matchValue (ValueType uri) o ctx = undefined
matchValue (ValueRef label) o ctx = 
   map typing (matchNode label (o2s o) ctx)

-- Utility function to create value sets from list of URIs
valueSet :: [URI] -> Value 
valueSet = ValueSet . Set.fromList . map ObjectURI

decompByPredicate :: Predicate -> Set Triple -> [(Triple, Set Triple)]
decompByPredicate p = decompBy (\t -> predicate t == p) 

