module Haws.ShEx.TestSemantics where

import Haws.ShEx.Shape
import Haws.ShEx.Result
import Haws.ShEx.Semantics
import Haws.ShEx.Typing hiding (main,tests)
import Haws.ShEx.RDFModel
import qualified Test.HUnit as Test
import Data.Set (Set)
import qualified Data.Set as Set
import Haws.Monads.BackMonad

-- Rules

-- rab 

-- a,b
rab :: Rule
rab = And (Arc (nameIRI "a") (ValueType xsd_string) noActions)
          (Arc (nameIRI "b") (ValueType xsd_string) noActions)
		
-- a|b		
raob :: Rule
raob = Or (Arc (nameIRI "a") (ValueType xsd_string) noActions)
          (Arc (nameIRI "b") (ValueType xsd_string) noActions)

-- a?b		  
raqb :: Rule
raqb = And (optional (Arc (nameIRI "a") (ValueType xsd_string) noActions))
           (Arc (nameIRI "b") (ValueType xsd_string) noActions)

-- a?b?
raqbq :: Rule
raqbq = And (optional (Arc (nameIRI "a") (ValueType xsd_string) noActions))
            (optional (Arc (nameIRI "b") (ValueType xsd_string) noActions))

-- a?b+
raqbp :: Rule
raqbp = And (optional (Arc (nameIRI "a") (ValueType xsd_string) noActions))
            (OneOrMore (Arc (nameIRI "b") (ValueType xsd_string) noActions))
			
-- Some instances		  
		
iab :: Set RDFTriple
iab = Set.fromList 
      [tripleStr ("x","a","_"),
       tripleStr ("x","b","_")
      ]
	  
iabb :: Set RDFTriple
iabb = Set.fromList 
      [tripleStr ("x","a","_"),
       tripleStr ("x","b","1"),
       tripleStr ("x","b","2")
	  ]

ia :: Set RDFTriple
ia = Set.fromList 
      [ tripleStr ("x","a","_")
      ]

ib :: Set RDFTriple
ib = Set.fromList 
      [ tripleStr ("x","b","_")
      ]

i0 :: Set RDFTriple
i0 = Set.fromList []
	  
test_iab_rab = Test.TestCase $ Test.assertEqual 
  "iab against rab"
  ( pass ) 
  ( matchRule ctx iab rab ) 

test_i0_rab = Test.TestCase $ Test.assertEqual 
  "i0 against rab"
  ( failure ) 
  ( matchRule ctx i0 rab ) 

test_ib_rab = Test.TestCase $ Test.assertEqual 
  "ib against rab"
  ( failure ) 
  ( matchRule ctx ib rab ) 

test_ia_rab = Test.TestCase $ Test.assertEqual 
  "ia against rab"
  ( failure ) 
  ( matchRule ctx ia rab ) 

-- raob

test_iab_raob = Test.TestCase $ Test.assertEqual 
  "a,b against a|b"
  ( failure ) 
  ( matchRule ctx iab raob ) 

test_i0_raob = Test.TestCase $ Test.assertEqual 
  "() against a|b"
  ( failure ) 
  ( matchRule ctx i0 raob ) 

test_ib_raob = Test.TestCase $ Test.assertEqual 
  "b against a|b"
  ( pass ) 
  ( matchRule ctx ib raob ) 

test_ia_raob = Test.TestCase $ Test.assertEqual 
  "a against a|b"
  ( pass ) 
  ( matchRule ctx ia raob ) 

-- raqb

test_iab_raqb = Test.TestCase $ Test.assertEqual 
  "ab against a?b"
  ( pass ) 
  ( matchRule ctx iab raqb ) 

test_i0_raqb = Test.TestCase $ Test.assertEqual 
  "() against a?b"
  ( failure ) 
  ( matchRule ctx i0 raqb ) 

test_ib_raqb = Test.TestCase $ Test.assertEqual 
  "b against a?b"
  ( pass ) 
  ( matchRule ctx ib raqb ) 

test_ia_raqb = Test.TestCase $ Test.assertEqual 
  "a against a?b"
  ( failure ) 
  ( matchRule ctx ia raqb ) 

-- raqbq

test_iab_raqbq = Test.TestCase $ Test.assertEqual 
  "ab against a?b?"
  ( pass ) 
  ( matchRule ctx iab raqbq ) 

test_i0_raqbq = Test.TestCase $ Test.assertEqual 
  "() against a?b?"
  ( pass ) 
  ( matchRule ctx i0 raqbq ) 

test_ib_raqbq = Test.TestCase $ Test.assertEqual 
  "b against a?b?"
  ( pass ) 
  ( matchRule ctx ib raqbq ) 

test_ia_raqbq = Test.TestCase $ Test.assertEqual 
  "a against a?b?"
  ( pass ) 
  ( matchRule ctx ia raqbq ) 

-- raqbp a?b+

test_iab_raqbp = Test.TestCase $ Test.assertEqual 
  "ab against a?b+"
  ( pass ) 
  ( matchRule ctx iab raqbp ) 

test_i0_raqbp = Test.TestCase $ Test.assertEqual 
  "() against a?b+"
  ( failure ) 
  ( matchRule ctx i0 raqbp ) 

test_ib_raqbp = Test.TestCase $ Test.assertEqual 
  "b against a?b+"
  ( pass ) 
  ( matchRule ctx ib raqbp ) 

test_ia_raqbp = Test.TestCase $ Test.assertEqual 
  "a against a?b+"
  ( failure ) 
  ( matchRule ctx ia raqbp ) 

test_iabb_raqbp = Test.TestCase $ Test.assertEqual 
  "abb against a?b+"
  ( pass ) 
  ( matchRule ctx iab raqbp ) 
  
pass :: Result Bool
pass = return True


ctx = Context { 
 graph  = RDFGraph (Set.fromList []),
 typing = emptyTyping
}
---


tests = Test.TestList 
 [ test_iab_rab
 , test_ia_rab
 , test_ib_rab
 , test_i0_rab
 , test_iab_raob
 , test_ia_raob
 , test_ib_raob
 , test_i0_raob
 , test_iab_raqb
 , test_ia_raqb
 , test_ib_raqb
 , test_i0_raqb
 , test_iab_raqbq
 , test_ia_raqbq
 , test_ib_raqbq
 , test_i0_raqbq
 , test_iab_raqbp
 , test_ia_raqbp
 , test_ib_raqbp
 , test_i0_raqbp
 , test_iabb_raqbp
 ]

main = Test.runTestTT tests
  