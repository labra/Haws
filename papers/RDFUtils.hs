module RDFUtils where
import RDF
import Namespaces
import Data.Set (Set)
import qualified Data.Set as Set


noTriples :: Set Triple
noTriples = Set.empty

foldGraph :: (Triple -> a -> a) -> a -> Graph -> a
foldGraph f e (Graph ts) = Set.foldr f e ts

triple :: (String,String,String) -> Triple
triple (s,p,o) = Triple { subject = uri_s s, predicate = uri_p p, object = uri_o o }

tripleLit :: (String,String,String) -> Triple
tripleLit (s,p,o) = Triple { subject = uri_s s, predicate = uri_p p, object = str_o o }

str_s :: String -> Literal
str_s s = DatatypeLiteral { lexicalForm = s, datatype = xsd_string }

str_o :: String -> Object
str_o s = ObjectLiteral (str_s s)

mktriples :: [(String,String,String)] -> Set Triple
mktriples = Set.fromList . 
		    map triple
