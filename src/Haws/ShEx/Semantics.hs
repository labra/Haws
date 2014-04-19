module Haws.ShEx.Semantics where
import Haws.ShEx.Shape
import Haws.ShEx.RDFModel
import Haws.ShEx.Result
import Data.Set (Set)
import qualified Data.Set as Set
import Haws.ShEx.Typing
import Haws.Monads.BackMonad

data Context = Context { 
   graph :: RDFGraph ,
   typing :: Typing
}

-- Semantics of ShEx

validateShEx :: ShEx -> RDFGraph -> Context -> Result Typing
validateShEx shex graph ctx = undefined


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
 else failure
 
matchRule ctx g (Arc n v _) =
 if (Set.size g == 1) then
  let t = head (Set.elems g)
  in 
   do
    matchName ctx (predicate t) n
    matchValue ctx (object t) v
 else
  failure     
  
matchName :: Context -> IRI -> NameClass -> Result Bool
matchName ctx pred (NameTerm term) = 
 if (pred == term) then return True
 else failure
matchName ctx pred (NameWild excl) =
 undefined
matchName ctx pred (NameStem stem) =
 undefined
 

matchValue :: Context -> Object -> ValueClass -> Result Bool
matchValue ctx obj (ValueType t) = 
 case datatypeObject obj of
  Just d  -> if d == t then return True
             else failure
  Nothing -> failure
  
matchValue ctx obj (ValueSet set) = 
  if Set.member obj set  
  then return True
  else failure
     
matchValue ctx obj (ValueWild excl) = undefined
matchValue ctx obj (ValueStem stem) = undefined
