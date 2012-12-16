module Main where

import Haws.RDFGraph
import Haws.TGraph

mkGraph :: RDFGraph
mkGraph = Exists $ \x -> 
          Exists $ \y -> 
          insertTripleRDF (IRI "a", IRI "b", BNode x) $
          insertTripleRDF (IRI "a", IRI "b", BNode y) $
          insertTripleRDF (BNode x, IRI "b", IRI "c") $ 
          emptyRDF 

main::IO()
main = do let g = mkGraph
          putStrLn "Hi! from Haws" 
          putStrLn $ "Folds:   " ++ (printRDFFolds g)
          