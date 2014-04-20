module Haws.ShEx.Semantics where
import Haws.ShEx.Shape
import Haws.ShEx.RDFModel
import Haws.ShEx.Result
import Data.Set (Set)
import qualified Data.Set as Set
import Haws.ShEx.Typing
import Haws.ShEx.Context
import Haws.Monads.BackMonad

-- Semantics of ShEx
-- This is a very naïve validation algorithm by brute force. 
-- It does not keep state of validations...every iri is re-validated every time
-- TODO:
--   Memoization of validations.
--   Implement a reader monad to obtain values from rdfgraph
--   Implement a writer monad to maintain typings

validateShEx :: ShEx -> RDFGraph -> Context -> Result Typing
validateShEx shex graph ctx = undefined

matchIRI :: Context -> IRI -> Shape -> Result Bool
matchIRI ctx iri shape = 
 do
  let triples = arcs iri (graph ctx)
  matchRule ctx triples (rule shape)
  
-- Match rule
matchRule :: Context -> Set RDFTriple -> Rule -> Result Bool
matchRule ctx g (And r1 r2) =
 do
  (g1,g2) <- parts g 
  matchRule ctx g1 r1
  matchRule ctx g2 r2
  return True
  
matchRule ctx g (Or r1 r2) =
 matchRule ctx g r1 `orelse` 
 matchRule ctx g r2

matchRule ctx g rule@(OneOrMore r) =
 matchRule ctx g r `orelse`
 do
  (g1,g2) <- parts g
  matchRule ctx g1 r
  matchRule ctx g2 rule 

matchRule ctx g EmptyRule =
 if (Set.null g) then return True
 else failure ("EmptyRule: graph non empty")
 
matchRule ctx g (Arc n v _) =
 if (Set.size g == 1) then
  let t = head (Set.elems g)
  in 
   do
    matchName ctx (predicate t) n
    matchValue ctx (object t) v
 else
  failure "matchRule: Arc expected but more than one triples found"
  
matchName :: Context -> IRI -> NameClass -> Result Bool
matchName ctx pred (NameTerm term) = 
 if (pred == term) then return True
 else failure ("matchName: " ++ show pred ++ " != " ++  show term)
 
matchName ctx pred (NameWild excl) =
 undefined
matchName ctx pred (NameStem stem) =
 undefined
 

matchValue :: Context -> Object -> ValueClass -> Result Bool
matchValue ctx obj (ValueType t) = 
 case datatypeObject obj of
  Just d  -> if d == t then return True
             else failure ("matchValue: " ++ show obj ++ " has type " ++ show d ++ " and must be " ++ show t)
  Nothing -> 
   failure ("matchValue: " ++ show obj ++ " has no type " ++ show t)
  
matchValue ctx obj (ValueSet set) = 
  if Set.member obj set  
  then return True
  else 
   failure ("matchValue: " ++ show obj ++ " does not belong to " ++ show set)
     
matchValue ctx obj (ValueWild excl) = undefined
matchValue ctx obj (ValueStem stem) = undefined

matchValue ctx obj (ValueReference lbl) = 
 case iriObject obj of
  Just iri -> do 
      shape <- getShape lbl ctx
      matchIRI ctx iri shape
  Nothing -> failure ("matchValue: object " ++ show obj ++ " must be a IRI")


getShape :: Label -> Context -> Result Shape
getShape lbl ctx = 
 case findShape lbl (shEx ctx) of
    Just shape -> return shape
    Nothing    -> failure ("getShape: not found shape with label " ++ show lbl)
