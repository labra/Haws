module Haws.ShEx.RDFModel where

data RDFGraph = RDFGraph [RDFTriple]
 deriving Show

data RDFTriple = RDFTriple { 
        subject   :: Subject, 
        predicate :: IRI, 
        object    :: Object 
     }
 deriving (Show, Eq, Ord)

data Subject = SubjIRI   { subjIRI  :: IRI   } 
             | SubjBNode { subBNode :: BNode }
 deriving (Show,Eq,Ord)

data Object  = ObjIRI { objIRI :: IRI } 
             | ObjLiteral { objLiteral :: Literal } 
             | ObjBNode { objBNode :: BNode }
 deriving (Show,Eq,Ord)

tripleIRIs :: (String,String,String) -> RDFTriple
tripleIRIs (s,p,o) = RDFTriple { 
  subject = SubjIRI (IRI s), 
  predicate = IRI p, 
  object = ObjIRI (IRI o)
} 

data BNode = BNode Int
 deriving (Show,Eq,Ord)
              
data IRI     = IRI String
 deriving (Show, Eq, Ord)

data Literal = DataTypeLiteral { lexicalForm::String, datatype:: IRI }
             | LangLiteral { str:: String, lang::String }
 deriving (Show,Eq,Ord)             

xsd :: String
xsd = "http://www.w3.org/2001/XMLSchema#"

xsd_string :: IRI
xsd_string = IRI (xsd ++ "string") 
 
strLiteral :: String -> Literal
strLiteral str = DataTypeLiteral str xsd_string


arcs :: IRI -> RDFGraph -> [(IRI, Object)]
arcs iri (RDFGraph triples) = 
   map(\t -> (predicate t, object t)) 
 ( filter(\t -> hasSubject t iri) 
   triples
 )
 
hasSubject :: RDFTriple -> IRI -> Bool
hasSubject t iri = equalsSubject (subject t) iri
 
equalsSubject :: Subject -> IRI -> Bool
equalsSubject (SubjIRI iri) iri2 = iri == iri2
equalsSubject _ _ = False   