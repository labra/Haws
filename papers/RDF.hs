-- Common RDF definitions

module RDF(
	URI,u,
	BNode,Literal,Subject(..),Predicate, Object(..), 
	Triple(..), Graph(..), emptyGraph, Node,
	uri_s, uri_p, bnode_s,bnode_o, uri_o,o2s,
	surroundingTriples,
	noTriples,
	same_object,
	triple, mktriples
	) where
import Data.List(intersperse)
import Data.Either(Either(..))

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map


-- Preliminary definitions
data URI = URI String 
 deriving (Eq,Ord)

instance Show URI where
 show (URI uri) = "<" ++ uri ++ ">"

-- Simple function to facilitate creating URIs
u :: String -> URI
u s = URI s
 
data BNode = BNode String
 deriving (Eq,Ord)
 
instance Show BNode where
 show (BNode b) = "_:" ++ b

data Literal = 
 Literal { 
  lexicalForm :: String , 
  datatype :: URI 
 }
 deriving (Eq,Ord,Show)

data Subject = SubjectURI URI 
             | SubjectBNode BNode
	deriving (Eq,Ord)
			 
instance Show Subject where 
 show (SubjectURI uri) = show uri
 show (SubjectBNode b) = show b

type Predicate = URI

data Object = ObjectURI URI
            | ObjectBNode BNode 
			| ObjectLiteral Literal
	deriving (Eq,Ord)

instance Show Object where 
 show (ObjectURI uri) = show uri
 show (ObjectBNode b) = show b
 show (ObjectLiteral l) = show l
 

o2s :: Object -> Subject
o2s (ObjectURI uri) 	= SubjectURI uri
o2s (ObjectBNode bnode) = SubjectBNode bnode
o2s (ObjectLiteral l)   = error ("o2s: cannot convert " ++ show l ++ " to subject")

is_uri :: Object -> Bool
is_uri (ObjectURI _) = True
is_uri _ = False

is_bNode :: Object -> Bool
is_bNode (ObjectBNode _) = True
is_bNode _ = False

is_Literal :: Object -> Bool
is_Literal (ObjectLiteral _) = True
is_Literal _ = False

uri_s :: String -> Subject
uri_s = SubjectURI . u 

uri_p :: String -> Predicate
uri_p s = u s

bnode_s :: String -> Subject
bnode_s = SubjectBNode . BNode 

uri_o :: String -> Object
uri_o = ObjectURI . u 

bnode_o :: String -> Object
bnode_o = ObjectBNode . BNode

-- literal_o :: Literal -> Object
-- literal_o l = Right (Right l)

data Triple = 
 Triple { 
   subject :: Subject
 , predicate:: Predicate
 , object:: Object
 }
 deriving (Eq,Ord)
 
instance Show Triple where
 show t = "[" ++ show (subject t) ++ "," ++ show (predicate t) ++ "," ++ show (object t) ++ "]"

data Graph = Graph { triples :: Set Triple }
 deriving (Eq,Ord)
 
instance Show Graph where 
 show (Graph ts) = Set.foldr (\t s -> show t ++ "\n" ++ s) "" ts

emptyGraph = Graph {triples = Set.empty }

type Node = Subject

same_subject :: Node -> Triple -> Bool
same_subject s t = s == subject t

same_object :: Node -> Triple -> Bool
same_object n t = 
     if is_uri o || is_bNode o 
	 then o2s o == n
	 else False
  where o = object t

-- shape finds the triples that surround a node in a graph
surroundingTriples :: Node -> Graph -> Set Triple
surroundingTriples n (Graph ts) = 
	Set.filter (\t -> same_subject n t || same_object n t) ts
	
noTriples :: Set Triple
noTriples = Set.empty

foldGraph :: (Triple -> a -> a) -> a -> Graph -> a
foldGraph f e (Graph ts) = Set.foldr f e ts

triple :: (String,String,String) -> Triple
triple (s,p,o) = Triple { subject = uri_s s, predicate = uri_p p, object = uri_o o }

mktriples :: [(String,String,String)] -> Set Triple
mktriples = Set.fromList . 
		    map triple
