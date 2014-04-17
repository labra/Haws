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

import Data.Set
import Haws.ShEx.RDFModel

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
                   
data ShEx = ShEx { rules :: [Shape]
                 , start :: Maybe [Label]
                 }                   

-- Rules
data Rule = Or Rule Rule
          | And Rule Rule
          | Group Rule Card Actions
          | Arc NameClass ValueClass Card Actions
          | EmptyRule
 deriving (Show,Eq)
 
data NameClass = NameTerm { term :: IRI }
               | NameWild  { excl :: Set [IRI] }
               | NameStem { stem :: IRI }
 deriving (Show, Eq)

data ValueClass = ValueType { v :: IRI }
                | ValueSet  { set :: Set [IRI] } 
                | ValueWild  { any ::Set[IRI] }
                | ValueStem { stem:: IRI }
                | ValueReference { ref :: Label }
 deriving (Show, Eq)
                
data Unbound = Unbound
 deriving (Show,Eq)


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

-----------------
-- Actions
 
data Action = Action String
 deriving (Show, Eq)
 
data Actions = Actions [Action]
 deriving (Show,Eq)            

noActions :: Actions
noActions = Actions []

-- Results 
data ShapeAsignment = CanBe [IRI]
                    | CannotBe [IRI]
 
data Typing = Map IRI ShapeAsignment

data Context = Context { 
   graph :: RDFGraph ,
   typing :: Typing
 }

-- Semantics of ShEx

validateShEx :: ShEx -> RDFGraph -> Context -> [Typing]
validateShEx shex graph ctx = undefined

validateIRI :: ShEx -> IRI -> Context -> [Typing]
validateIRI shex iri ctx = undefined


