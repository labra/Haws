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

module Haws.ShEx (

) where

import Data.Set

-- RDFModel

data RDFGraph = RDFGraph [RDFTriple]
 deriving Show

data RDFTriple = RDFTriple { 
        subject::RDFNode, 
        predicate::IRI, 
        object::Object 
     }
 deriving (Show, Eq, Ord)

type RDFNode = Either IRI BNode

type Object  = Either IRI (Either BNode Literal)

data BNode = BNode Int
 deriving (Show,Eq,Ord)
              
data IRI     = IRI String
 deriving (Show, Eq, Ord)

data Literal = DataTypeLiteral { lexicalForm::String, datatype:: IRI }
             | LangLiteral { str:: String, lang::String }
 deriving (Show,Eq,Ord)             
---------------------
-- ShEx model

-- Labels

data Label = Label IRI
 deriving (Show,Eq)

mkLabel :: String -> Label
mkLabel str = Label (IRI str)


-- Rules
data Rule = OrRule Rule Rule
          | AndRule Rule Rule
          | ArcRule NameClass ValueClass Card Actions
          | EmptyRule
 deriving (Show,Eq)
 
data NameClass = NameTerm { t :: IRI }
               | NameWild  { excl :: Set [IRI] }
               | NameStem { s :: IRI }
 deriving (Show, Eq)

data ValueClass = ValueType { v :: IRI }
                | ValueSet  { set :: Set [IRI] } 
                | ValueWild  { any ::Set[IRI] }
                | ValueStem { stem:: IRI }
                | ValueReference { label::Label }
 deriving (Show, Eq)
                
data Unbound = Unbound
 deriving (Show,Eq)


-- Cardinalities
data Card = Card { minCard:: Int, maxCard :: Either Int Unbound }
 deriving (Show, Eq)
 
defaultCard :: Card
defaultCard = Card { minCard = 1, maxCard = Left 1 }

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

-- Examples

resource :: String -> RDFNode
resource str = Left (IRI str)

arc1 :: Rule
arc1 = ArcRule (NameTerm (IRI ":name"))
               (ValueType (IRI "xsd:string"))
               (defaultCard)
               (noActions) 
