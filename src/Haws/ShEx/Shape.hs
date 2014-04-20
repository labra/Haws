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

module Haws.ShEx.Shape where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List(permutations,find)
import qualified Test.HUnit as Test
import Haws.ShEx.RDFModel
import Haws.ShEx.Typing
import Haws.Monads.BackMonad

---------------------
-- ShEx model

-- Labels

data Label = Label IRI
 deriving (Show,Eq)

mkLabel :: String -> Label
mkLabel str = Label (IRI str)

-- Shapes

data Shape = Shape { label:: Label
                   , rule :: Rule
                   }
	deriving (Show)
                   
				   
data ShEx = ShEx { shapes :: [Shape] 
                 , start  :: Maybe [Label]
                 } 
    deriving (Show)
-- TODO: Implement ShEx as maps from Labels to Rules

findShape :: Label -> ShEx -> Maybe Shape
findShape lbl shEx = 
 find (\shape -> (label shape == lbl)) (shapes shEx)

-- Rules
data Rule = Or Rule Rule
          | And Rule Rule
     --   | Group Rule Card Actions
          | OneOrMore Rule
          | Arc NameClass ValueClass Actions
          | EmptyRule
 deriving (Show,Eq)
 
data NameClass = NameTerm IRI 
               | NameWild (Set [IRI])
               | NameStem IRI
 deriving (Show, Eq)


data ValueClass = ValueType IRI 
                | ValueSet  (Set Object)  -- TODO: check with specification 
                | ValueWild (Set [IRI])
                | ValueStem IRI 
                | ValueReference Label 
 deriving (Show, Eq)
                
data Unbound = Unbound
 deriving (Show,Eq)

-- Some utility functions 
nameIRI :: String -> NameClass
nameIRI str = NameTerm (IRI str) 

{-
-- Cardinalities
data Card = Card { minCard:: Int, maxCard :: Either Int Unbound }
 deriving (Show, Eq)
 
defaultCard :: Card
defaultCard = Card { minCard = 1, maxCard = Left 1 }

optionalCard :: Card
optionalCard = Card { minCard = 0, maxCard = Left 1 }

star :: Card
star = Card { minCard = 0, maxCard = Right Unbound }

plus :: Card
plus = Card { minCard = 1, maxCard = Right Unbound }

card :: (Int,Int) -> Card
card (m,n)  
 | m <= n    = Card { minCard = m, maxCard = Left n}
 | otherwise = error ("card: m > n where m = " ++ show m ++ ", n = " ++ show n) 
-}
-----------------
-- Actions
 
data Action = Action String
 deriving (Show, Eq)
 
data Actions = Actions [Action]
 deriving (Show,Eq)            

noActions :: Actions
noActions = Actions []

----
star :: Rule -> Rule
star r = Or (OneOrMore r) EmptyRule

optional :: Rule -> Rule
optional r = Or r EmptyRule
  
  
 
