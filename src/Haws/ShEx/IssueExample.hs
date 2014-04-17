module Haws.ShEx.IssueExample where

import Haws.ShEx.Shape
import Haws.ShEx.RDFModel

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
          (ValueType xsd_string)
          (defaultCard)
          (noActions))
     (Arc (NameTerm (IRI "mbox"))
          (ValueType xsd_string)
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
            (ValueType xsd_string)
            (defaultCard)
            (noActions)
       )
       (And
          (Arc (NameTerm (IRI "givenName"))
               (ValueType xsd_string)
               (star)
               (noActions)
          )
          (Arc (NameTerm (IRI "familyName"))
               (ValueType xsd_string)
               (defaultCard)
               (noActions)
          )
       )
     )
     ( Arc (NameTerm (IRI "mbox"))
           (ValueType xsd_string)
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
                    (ValueType xsd_string)
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
              
              
------------

empty :: RDFGraph 
empty = RDFGraph []              
     
s :: String -> Object
s str = strLiteral str      
  
issue1 :: [RDFTriple]
issue1 = 
 [ tripleIRIs ("issue1","state",s "assigned")
 , tripleIRIs ("issue1","reportedBy","john")
 , tripleIRIs ("john","name","john") 
 ]
 
 