-----------------------------------------------------------------------------
--
-- Module      :  Haws.ShEx
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Jose Labra
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Haws.ShEx.Typing where

import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Haws.ShEx.RDFModel
import qualified Test.HUnit as Test

-- Typing Results 
data Types a = Types { 
   hasTypes  :: Set.Set a, 
   forbidden :: Set.Set a 
 }
 deriving (Eq,Show)
 
emptyTypes :: Types a
emptyTypes = Types { hasTypes = Set.empty, forbidden = Set.empty }

singleType :: a -> Types a
singleType t = Types { hasTypes = Set.singleton t, forbidden = Set.empty }

singleForbidden :: a -> Types a
singleForbidden t = Types { hasTypes = Set.empty, forbidden = Set.singleton t }

addHasType :: (Monad m, Eq a, Show a, Ord a) => 
                a -> Types a -> m (Types a)
addHasType t ts =
  if (Set.member t (forbidden ts)) then   
    fail ("cannot assign type " ++ show t ++ ". It is in the forbidden list " ++ show ts)
  else
    return (ts {hasTypes = Set.insert t (hasTypes ts) }) 


type Typing = Map.Map IRI (Types IRI)

emptyTyping :: Typing 
emptyTyping = Map.empty

addType :: Monad m => IRI -> IRI -> Typing -> m Typing 
addType node iri typing =
 case Map.lookup node typing of 
  Nothing    -> return $ Map.insert node (singleType iri) typing
  Just types -> do 
                  newType <- addHasType iri types
                  return (Map.adjust (\t -> newType) node typing)   
                    
combineTypings :: Typing -> Typing -> Typing
combineTypings t1 t2 = Map.union t1 t2
-------------------------------------------------------
-- Unit tests


exType :: Types IRI
exType = Types { 
 hasTypes = Set.fromList [IRI "a", IRI "b"], 
 forbidden = Set.fromList [IRI "c"]
}

exTyped :: Types IRI
exTyped = Types { 
 hasTypes = Set.fromList [IRI "a", IRI "b", IRI "d"], 
 forbidden = Set.fromList [IRI "c"]
}


testAddTypeColisionForbidden = Test.TestCase $ Test.assertEqual 
  "Should get Nothing when adding a type and it appears in forbidden"
  Nothing 
  ( addHasType (IRI "c") exType ) 

testAddTypeNormal = Test.TestCase $ Test.assertEqual 
  "Add type happy case"
  ( Just  (exTyped)) 
  ( addHasType (IRI "d") exType ) 

testAddTypeRepeated = Test.TestCase $ Test.assertEqual 
  "Add type repeated"
  ( Just exType ) 
  ( addHasType (IRI "b") exType ) 


exType2 :: Types IRI
exType2 = Types { 
 hasTypes  = Set.fromList [IRI "d", IRI "e"], 
 forbidden = Set.fromList [IRI "f"]
}

typing1 = Map.fromList [(IRI "x", exType),(IRI "y",exType2)]
typing1d = Map.fromList [(IRI "x", exTyped),(IRI "y",exType2)]

testAddTypingHappy = Test.TestCase $ Test.assertEqual 
  "Add typing happy case"
  ( Just typing1d ) 
  ( addType (IRI "x") (IRI "d") typing1 ) 

testAddTypingRepeated = Test.TestCase $ Test.assertEqual 
  "Add typing repeated"
  ( Just typing1 ) 
  ( addType (IRI "x") (IRI "b") typing1 ) 

testAddTypingForbidden = Test.TestCase $ Test.assertEqual 
  "Add typing repeated"
  ( Nothing ) 
  ( addType (IRI "x") (IRI "c") typing1 ) 

---


tests = Test.TestList [
    testAddTypeColisionForbidden
  , testAddTypeNormal
  , testAddTypeRepeated
  , testAddTypingHappy   
  , testAddTypingRepeated
  , testAddTypingForbidden
  ]

main = Test.runTestTT tests
  