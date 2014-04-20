module Haws.ShEx.TestSemantics where

import Haws.ShEx.Shape
import Haws.ShEx.Result
import Haws.ShEx.Semantics
import Haws.ShEx.Context
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

-- (ab)?c+
r_ab_qcp :: Rule
r_ab_qcp = And 
            (optional 
              (And (Arc (nameIRI "a") (ValueType xsd_string) noActions)
                   (Arc (nameIRI "b") (ValueType xsd_string) noActions)))
		    (OneOrMore (Arc (nameIRI "c") (ValueType xsd_string) noActions))
			
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

ic :: Set RDFTriple
ic = Set.fromList [ tripleStr ("x","c","_") ]

i0 :: Set RDFTriple
i0 = Set.fromList []

iabc :: Set RDFTriple
iabc = Set.fromList 
      [ tripleStr ("x","a","_")
	  , tripleStr ("x","b","_")
	  , tripleStr ("x","c","_")
      ]
	  
iabcc :: Set RDFTriple
iabcc = Set.fromList 
      [ tripleStr ("x","a","_")
	  , tripleStr ("x","b","_")
	  , tripleStr ("x","c","1")
	  , tripleStr ("x","c","2")
      ]

test_iab_rab = Test.TestCase $ Test.assertBool
  "iab against rab" $
  isPassed $
  matchRule ctx iab rab 

test_i0_rab = Test.TestCase $ Test.assertBool
  "i0 against rab" $
  isFailure $ 
  matchRule ctx i0 rab 

test_ib_rab = Test.TestCase $ Test.assertBool
  "ib against rab" $
  isFailure $ 
  matchRule ctx ib rab 
  
test_ia_rab = Test.TestCase $ Test.assertBool
  "ia against rab" $
  isFailure $
  matchRule ctx ia rab 

-- raob

test_iab_raob = Test.TestCase $ Test.assertBool 
  "a,b against a|b" $
   isFailure $ 
   matchRule ctx iab raob 

test_i0_raob = Test.TestCase $ Test.assertBool 
  "() against a|b" $
  isFailure $
  matchRule ctx i0 raob 

test_ib_raob = Test.TestCase $ Test.assertBool 
  "b against a|b" $
  isPassed $
  matchRule ctx ib raob 

test_ia_raob = Test.TestCase $ Test.assertBool 
  "a against a|b" $
  isPassed $ 
  matchRule ctx ia raob 


-- raqb

test_iab_raqb = Test.TestCase $ Test.assertBool 
  "ab against a?b" $
  isPassed $
  matchRule ctx iab raqb 

test_i0_raqb = Test.TestCase $ Test.assertBool 
  "() against a?b" $
  isFailure $
  matchRule ctx i0 raqb 

test_ib_raqb = Test.TestCase $ Test.assertBool 
  "b against a?b" $
  isPassed $ 
  matchRule ctx ib raqb 

test_ia_raqb = Test.TestCase $ Test.assertBool 
  "a against a?b" $
  isFailure $
  matchRule ctx ia raqb 

-- raqbq

test_iab_raqbq = Test.TestCase $ Test.assertBool 
  "ab against a?b?" $
  isPassed $  
  matchRule ctx iab raqbq 

test_i0_raqbq = Test.TestCase $ Test.assertBool 
  "() against a?b?" $
  isPassed $  
  matchRule ctx i0 raqbq 

test_ib_raqbq = Test.TestCase $ Test.assertBool 
  "b against a?b?" $
  isPassed $  
  matchRule ctx ib raqbq 

test_ia_raqbq = Test.TestCase $ Test.assertBool 
  "a against a?b?" $
  isPassed $  
  matchRule ctx ia raqbq 

-- raqbp a?b+

test_iab_raqbp = Test.TestCase $ Test.assertBool 
  "ab against a?b+" $
  isPassed $  
  matchRule ctx iab raqbp 

test_i0_raqbp = Test.TestCase $ Test.assertBool 
  "() against a?b+" $
  isFailure $
  matchRule ctx i0 raqbp 

test_ib_raqbp = Test.TestCase $ Test.assertBool 
  "b against a?b+" $
  isPassed $  
  matchRule ctx ib raqbp 

test_ia_raqbp = Test.TestCase $ Test.assertBool 
  "a against a?b+" $
  isFailure $
  matchRule ctx ia raqbp 

test_iabb_raqbp = Test.TestCase $ Test.assertBool 
  "abb against a?b+" $
  isPassed $  
  matchRule ctx iab raqbp 

-- r_ab_qcp (ab)?c+

test_iab_r_ab_qcp = Test.TestCase $ Test.assertBool 
  "ab against (ab)?c+" $
  isFailure $
  matchRule ctx iab r_ab_qcp 

test_i0_r_ab_qcp = Test.TestCase $ Test.assertBool 
  "() against (ab)?c+" $
  isFailure $
  matchRule ctx i0 r_ab_qcp 

test_ib_r_ab_qcp = Test.TestCase $ Test.assertBool 
  "b against (ab)?c+" $
  isFailure $
  matchRule ctx ib r_ab_qcp 

test_ia_r_ab_qcp = Test.TestCase $ Test.assertBool 
  "a against (ab)?c+" $
  isFailure $
  matchRule ctx ia r_ab_qcp 

test_iabb_r_ab_qcp = Test.TestCase $ Test.assertBool 
  "abb against (ab)?c+" $
  isFailure $
  matchRule ctx iab r_ab_qcp 

test_ic_r_ab_qcp = Test.TestCase $ Test.assertBool 
  "c against (ab)?c+" $
  isPassed $  
  matchRule ctx ic r_ab_qcp 

test_iabc_r_ab_qcp = Test.TestCase $ Test.assertBool 
  "abc against (ab)?c+" $
  isPassed $  
  matchRule ctx iabc r_ab_qcp 
  
test_iabcc_r_ab_qcp = Test.TestCase $ Test.assertBool 
  "abcc against (ab)?c+" $
  isPassed $
  matchRule ctx iabcc r_ab_qcp 

-- Testing references

{- 
 l1 { <a> xsd:string, <b> @<l2> }
 l2 { <c> xsd:string }
 -}

rl1l2 = ShEx {
 shapes = [shapel1,shapel2],
 start  = Nothing
}

shapel1 = 
  Shape { label = mkLabel "l1",
          rule = 
		    And (Arc (nameIRI "a") (ValueType xsd_string) noActions)
                (Arc (nameIRI "b") (ValueReference (mkLabel "l2")) noActions)
	    }

shapel2 =  
  Shape { label = mkLabel "l2",
          rule  = Arc (nameIRI "c") (ValueType xsd_string) noActions
		}

g1 = RDFGraph $ Set.fromList [
  tripleStr ("x","a","_"),
  tripleIRIs ("x","b","y"),
  tripleStr ("y","c","_")
 ]

ctxg1_l1l2 = Context { 
 graph  = g1,
 shEx   = rl1l2,
 typing = emptyTyping
}

test_x_l1_rl1l2 = Test.TestCase $ Test.assertBool 
  "x against shapel1 in rl1l2 and graph g1" $
  isPassed $
  matchIRI ctxg1_l1l2 (IRI "x") (shapel1) 

test_x_l2_rl1l2 = Test.TestCase $ Test.assertBool 
  "x against shapel2 in rl1l2 and graph g1" $
  isFailure $
  matchIRI ctxg1_l1l2 (IRI "x") (shapel2) 

test_y_l1_rl1l2 = Test.TestCase $ Test.assertBool 
  "y against shapel1 in rl1l2 and graph g1" $
  isFailure $
  matchIRI ctxg1_l1l2 (IRI "y") (shapel1) 

test_y_l2_rl1l2 = Test.TestCase $ Test.assertBool 
  "y against shapel2 in rl1l2 and graph g1" $
  isPassed $
  matchIRI ctxg1_l1l2 (IRI "y") (shapel2) 

----------------------------------------------------  


ctx = Context { 
 graph  = RDFGraph (Set.fromList []),
 shEx   = ShEx { shapes = [], start = Nothing },
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
 , test_iab_r_ab_qcp
 , test_ia_r_ab_qcp
 , test_ib_r_ab_qcp
 , test_i0_r_ab_qcp
 , test_iabb_r_ab_qcp
 , test_ic_r_ab_qcp
 , test_iabc_r_ab_qcp
 , test_iabcc_r_ab_qcp 
 , test_x_l1_rl1l2
 , test_x_l2_rl1l2
 , test_y_l1_rl1l2
 , test_y_l2_rl1l2 
 ]

main = Test.runTestTT tests
  