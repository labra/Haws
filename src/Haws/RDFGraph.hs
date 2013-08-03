module Haws.RDFGraph where

import Haws.TGraph
import Haws.TContext
import Haws.FGLTGraph
import Haws.Unify
import Data.Set

data Resource = IRI String
              | Lit String
              | BNode BNodeId
              deriving (Show, Eq, Ord)

data RDFGraph = Basic (FGLTGraph Resource)
              | Exists (BNodeId -> RDFGraph)
             
instance Show RDFGraph where 
 show g = showRDF 1 g
 
showRDF :: BNodeId -> RDFGraph -> String
showRDF _ (Basic g) = 
  (Data.Set.foldr (\t r -> (show t) ++ "\n" ++ r) "" (triples g)) ++ "\n"
showRDF n (Exists f) = "Exists " ++ show n ++ " (" ++ showRDF (n + 1) (f n) ++ ")"

emptyRDF :: RDFGraph
emptyRDF = Basic gEmpty

insertTripleRDF :: (Resource, Resource, Resource) -> RDFGraph -> RDFGraph
insertTripleRDF (s,p,o) (Basic g)  = Basic (insertTriple (s,p,o) g)
insertTripleRDF (s,p,o) (Exists f) = Exists (\bnode -> insertTripleRDF (s,p,o) (f bnode))

insertTriplesRDF :: Set (Resource,Resource,Resource) -> RDFGraph -> RDFGraph
insertTriplesRDF ts g = Data.Set.foldr insertTripleRDF g ts

triplesRDF :: RDFGraph -> Set (Resource,Resource,Resource)
triplesRDF g = triplesRDFfrom 0 g

triplesRDFfrom :: Int -> RDFGraph -> Set (Resource,Resource,Resource)
triplesRDFfrom _ (Basic g) = triples g
triplesRDFfrom n (Exists f) = triplesRDFfrom (n + 1) (f n)            

compRDF :: TContext Resource -> RDFGraph -> RDFGraph
compRDF ctx (Exists f) = Exists (\x -> compRDF ctx (f x))
compRDF ctx (Basic g) = Basic (comp ctx g)

mergeRDF :: RDFGraph -> RDFGraph -> RDFGraph
mergeRDF g (Exists f) = Exists (\x -> mergeRDF g (f x))
mergeRDF g (Basic g1) = foldTGraph g (\ctx g' -> compRDF ctx g') g1

containsIRI :: RDFGraph -> Resource -> Bool
containsIRI (Exists f) iri@(IRI _) = containsIRI (f 0) iri
containsIRI (Basic g) iri@(IRI _) = contains iri g
containsIRI _ _ = error "containsIRI only accepts IRIs"

foldRDFGraphOrd :: a -> (TContext Resource -> a -> a) -> RDFGraph -> a
foldRDFGraphOrd e h = foldRDFGraphOrdAux e h 0

foldRDFGraphOrdAux e h seed (Basic g) = foldTGraphOrd e h g 
foldRDFGraphOrdAux e h seed (Exists f)= foldRDFGraphOrdAux e h (seed + 1) (f seed)

foldRDFGraph :: a -> (TContext Resource -> a -> a) -> RDFGraph -> a
foldRDFGraph e h = foldRDFGraph' e h 0
 where 
  foldRDFGraph' e h s (Basic g)  = foldTGraph e h g 
  foldRDFGraph' e h s (Exists f) = foldRDFGrap' e h (s + 1) (f seed)

mapRDFGraph::(Resource -> Resource) -> RDFGraph -> RDFGraph
mapRDFGraph h (Basic g) = Basic (gmapTGraph (mapCtx h) g)
mapRDFGraph h (Exists f) = Exists (\x -> mapRDFGraph h (f x))

printRDFFolds :: RDFGraph -> String
printRDFFolds = foldRDFGraphOrd "Empty" (\ctx r -> show ctx ++ "\n" ++ r)

type BNodeId = Int

type RDFContext = TContext Resource

isBNode :: RDFContext -> Bool
isBNode ctx = case node ctx of
 BNode _ -> True
 _       -> False


g1 :: RDFGraph
g1 = insertTripleRDF (IRI "1", IRI "2", IRI "3") emptyRDF 

g2 :: RDFGraph
g2 = Exists (\x -> insertTripleRDF (IRI "1", IRI "2", BNode x) emptyRDF )

g3 = Exists (\x -> insertTripleRDF (IRI "2", IRI "3", BNode x) g2)

g4 = Exists (\x -> Exists (\y -> 
        insertTripleRDF (IRI "a", IRI "b", BNode x)
        ( insertTripleRDF (IRI "a", IRI "b", BNode y)
        ( insertTripleRDF (BNode x, IRI "b", IRI "c") emptyRDF
        ))
      ))

g5 = Exists $ 
      \x -> Exists $
      \y -> insertTripleRDF (IRI "a", IRI "b", BNode x) $
            insertTripleRDF (BNode x, IRI "b", IRI "d") $
            insertTripleRDF (BNode x, IRI "b", IRI "e") $
            insertTripleRDF (IRI "a", IRI "b", IRI "c") $
            emptyRDF


