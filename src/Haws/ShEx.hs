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
 
data NameClass = NameTerm { t :: IRI }
               | NameWild  { excl :: Set [IRI] }
               | NameStem { s :: IRI }
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

-- Examples

resource :: String -> RDFNode
resource str = Left (IRI str)



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


--------------------------------------------
---- Examples

{- Employee

<EmployeeShape> {
  foaf:name xsd:string
, foaf:mbox xsd:string
} 
-}


employeeShape :: Shape
employeeShape = 
 Shape {
   label = mkLabel("employee"),
   rule = And
     (Arc (NameTerm (IRI "name"))
          (ValueType (IRI "xsd:string"))
          (defaultCard)
          (noActions))
     (Arc (NameTerm (IRI "mbox"))
          (ValueType (IRI "xsd:string"))
          (defaultCard)
          (noActions))
   }
 
{- User

<UserShape> {
 ( foaf:name xsd:string
 | ( foaf:givenName  xsd:string+
   , foaf:familyName xsd:string
   )
 )
 , foaf:mbox xsd:string
}

-}


userShape :: Shape
userShape = 
 Shape {
   label = mkLabel("user"),
   rule = 
    And
     (Or 
       (Arc (NameTerm (IRI "name"))
            (ValueType (IRI "xsd:string"))
            (defaultCard)
            (noActions)
       )
       (And
          (Arc (NameTerm (IRI "givenName"))
               (ValueType (IRI "xsd:string"))
               (star)
               (noActions)
          )
          (Arc (NameTerm (IRI "familyName"))
               (ValueType (IRI "xsd:string"))
               (defaultCard)
               (noActions)
          )
       )
     )
     ( Arc (NameTerm (IRI "mbox"))
           (ValueType (IRI "xsd:string"))
           (defaultCard)
           (noActions)
     )
 }


{- Issue shapes

<IssueShape> {
   ex:state (ex:unassigned ex:assigned)
 , ex:reportedBy @<UserShape>
 , ex:reportedOn xsd:dateTime
 , ( ex:reproducedBy @<EmployeeShape>
   , ex:reproducedOn xsd:dateTime      
   )?
 , ex:related @<IssueShape>*
}

-}


issueShape :: Shape
issueShape = 
 Shape {
   label = mkLabel("issue"),
   rule = 
    And
     (Arc (NameTerm (IRI "state"))
          (ValueType (IRI "assigned"))
          (defaultCard)
          (noActions))
     (And 
       (Arc (NameTerm (IRI "reportedBy"))
            (ValueReference (mkLabel "userShape"))
            (defaultCard)
            (noActions)
       )
       (Group (And 
                (Arc (NameTerm (IRI "reproducedBy"))
                    (ValueReference (mkLabel "employeeShape"))
                    (defaultCard)
                    (noActions)
                )
                (Arc (NameTerm (IRI "reproducedOn"))
                    (ValueType (IRI "xsd:string"))
                    (defaultCard)
                    (noActions)
                )
              ) 
              optionalCard
              noActions
       ) -- Group
     ) -- And
  }

schema :: ShEx
schema = ShEx { rules = [employeeShape, userShape, issueShape]
              , start = Nothing
              } 