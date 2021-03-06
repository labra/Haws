module TestShacl where

import Shacl
import Test.Framework 
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Node)
import Data.Set (Set)
import qualified Data.Set as Set
import RDF
import RDFUtils
import Namespaces
import Sets
import Typing

test_combineTyping1 = combineTypings typing1 typing2 @?= typing
 where typing1 = singleTyping (uri_s ":s") (u "shapeC") 
       typing2 = singleTyping (uri_s ":s") (u "shapeB") 
       typing = addType (uri_s ":s") (u "shapeC") 
	          ( singleTyping (uri_s ":s") (u "shapeB") 
			  )
 
test_surrounding_1 = surroundingTriples (uri_s ":a") graph1 @?= surrounding_1
 where graph1 = Graph( mktriples [ (":a", ":p", ":c")
                          , (":c", ":r", ":e")
                          , (":a", ":q", ":d")
                          , (":d", ":r", ":f")
                          ])
       surrounding_1 = mktriples [ (":a", ":p", ":c") 
                                 , (":a", ":q", ":d")
                                 ]

test_surrounding_2 = surroundingTriples (uri_s ":a") graph2 @?= surrounding_2
 where graph2 = Graph (mktriples [ (":a", ":p", ":c")
                          , (":a", ":q", ":d")
                          , (":c", ":r", ":a")
                          ])
       surrounding_2 = mktriples [ (":a", ":p", ":c")
                                , (":a", ":q", ":d")
                                , (":c", ":r", ":a")
                                ]
  


test_same_object = same_object (uri_s ":a") (triple (":c", ":r", ":a")) @?= True

-- Schemas

test_empty = validate node label ctx @?= [s]
 where 
  schema = Schema (Set.fromList [(label, Empty)])
  node = uri_s ":a"
  label = u "label"
  cs = noTriples
  rs = ts
  graph = Graph ts
  ts = mktriples [(":a", ":b", ":c")]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = cs, remaining = rs, typing = singleTyping node label }


test_closed_empty_fail = validate node label ctx @?= []
 where 
  schema = Schema (Set.fromList [(label, Closed Empty)])
  node = uri_s ":a"
  label = u "label"
  cs = noTriples
  rs = ts
  graph = Graph ts
  ts = mktriples [(":a", ":b", ":c")]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
 
test_closed_empty_succeed_empty = validate node label ctx @?= [s]
 where 
  schema = Schema (Set.fromList [(label, Closed Empty)])
  node = uri_s ":a"
  label = u "label"
  cs = noTriples
  rs = ts
  graph = Graph ts
  ts = mktriples []
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = noTriples, remaining = noTriples, typing = singleTyping node label }

test_closed_empty_succeed_no_surrounding = validate node label ctx @?= [s]
 where 
  schema = Schema (Set.fromList [(label, Closed Empty)])
  node = uri_s ":a"
  label = u "label"
  cs = noTriples
  rs = ts
  graph = Graph ts
  ts = mktriples [(":b", ":p", ":c")]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = noTriples, remaining = noTriples, typing = singleTyping node label }

test_matchArc_1 = matchArc p vo t ctx @?= [emptyTyping]
 where
  p = u ":p"
  vo = valueSet [u ":a1",u ":a2"]
  cs = noTriples
  t = triple (":x", ":p", ":a1")
  rs = noTriples
  typing = singleTyping (uri_s ":x") (u "label")
  ctx = Context { schema = emptySchema, graph = emptyGraph, currentTyping = emptyTyping }
  s = ValidationState { checked = cs, remaining = rs, typing = emptyTyping }
  s' = s { checked = insert t cs }

  
test_arc_single = validate node label ctx @?= [s]
 where 
  schema = Schema (Set.fromList [(label, Arc (u ":p") (valueSet [u ":a1",u ":a2"]) plus)])
  node = uri_s ":x"
  label = u "label"
  cs = Set.fromList [t]
  rs = noTriples
  graph = Graph ts
  t = triple (":x", ":p", ":a1")
  ts = Set.fromList [t]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = cs, remaining = rs, typing = singleTyping node label }


test_arc_single_two = validate node label ctx @?= [s] 
 where 
  schema = Schema (Set.fromList [(label, Arc (u ":p") (valueSet [u ":a1",u ":a2"]) plus)])
  node = uri_s ":x"
  label = u "label"
  cs = Set.fromList [t1]
  rs = Set.fromList [t2]
  graph = Graph ts
  t1 = triple (":x", ":p", ":a1")
  t2 = triple (":x", ":q", ":y")
  ts = Set.fromList [t1,t2]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = cs, remaining = rs, typing = singleTyping node label }
  
test_arc_12_1ok_1bad = validate node label ctx @?= [s]
 where 
  schema = Schema (Set.fromList [(label, Arc (u ":p") (valueSet [u ":a1",u ":a2"]) plus)])
  node = uri_s ":x"
  label = u "label"
  cs = Set.fromList [t1]
  rs = Set.fromList [t2]
  graph = Graph ts
  t1 = triple (":x", ":p", ":a1")
  t2 = triple (":x", ":q", ":y")
  ts = Set.fromList [t1,t2]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = cs, remaining = rs, typing = singleTyping node label }

test_arc_12_2ok = head (validate node label ctx) @?= s
 where 
  schema = Schema (Set.fromList [(label, Arc (u ":p") (valueSet [u ":a1",u ":a2"]) (Range 1 2))])
  node = uri_s ":x"
  label = u "label"
  cs = Set.fromList [t1,t2]
  rs = Set.fromList []
  graph = Graph ts
  t1 = triple (":x", ":p", ":a1")
  t2 = triple (":x", ":p", ":a2")
  ts = Set.fromList [t1,t2]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = cs, remaining = rs, typing = singleTyping node label }

test_arc_2_2ok = validate node label ctx @?= [s1,s2]
 where 
  schema = Schema (mkset [(label, Arc (u ":p") (valueSet [u ":a1",u ":a2"]) (Range 2 2))])
  node = uri_s ":x"
  label = u "label"
  cs = mkset [t1,t2]
  rs = mkset []
  graph = Graph ts
  t1 = triple (":x", ":p", ":a1")
  t2 = triple (":x", ":p", ":a2")
  ts = mkset [t1,t2]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s1 = ValidationState { checked = cs, remaining = rs, typing = singleTyping node label }
  s2 = ValidationState { checked = cs, remaining = rs, typing = singleTyping node label }


test_arc_12_3_fail = validate node label ctx @?= []
 where 
  schema = Schema (Set.fromList [(label, Arc (u ":p") (valueSet [u ":a1",u ":a2",u ":a3"]) (Range 1 2))])
  node = uri_s ":x"
  label = u "label"
  graph = Graph ts
  t1 = triple (":x", ":p", ":a1")
  t2 = triple (":x", ":p", ":a2")
  t3 = triple (":x", ":p", ":a3")
  ts = Set.fromList [t1,t2,t3]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 

test_arc_23_1_fail = validate node label ctx @?= []
 where 
  schema = Schema (Set.fromList [(label, Arc (u ":p") (valueSet [u ":a1",u ":a2",u ":a3"]) (Range 2 3))])
  node = uri_s ":x"
  label = u "label"
  graph = Graph ts
  t1 = triple (":x", ":p", ":a1")
  ts = Set.fromList [t1]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  
test_arc_11 = validate node label ctx @?= [s] 
 where 
  schema = Schema (Set.fromList [(label, Arc (u ":p") (valueSet [u ":a1",u ":a2"]) (Range 1 1))])
  node = uri_s ":x"
  label = u "label"
  graph = Graph ts
  cs = ts
  rs = noTriples
  t1 = triple (":x", ":p", ":a1")
  ts = Set.fromList [t1]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = cs, remaining = rs, typing = singleTyping node label }

test_arc_11_rem = validate node label ctx @?= [s] 
 where 
  schema = Schema (mkset [(label, Arc (u ":p") (valueSet [u ":a1",u ":a2"]) (Range 1 1))])
  node = uri_s ":x"
  label = u "label"
  graph = Graph ts
  cs = mkset [t1]
  rs = mkset [t2]
  t1 = triple (":x", ":p", ":a1")
  t2 = triple (":x", ":q", ":c1")
  ts = mkset [t1,t2]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = cs, remaining = rs, typing = singleTyping node label }

test_and_12 = validate node label ctx @?= [s] 
 where 
  schema = Schema (Set.fromList [(label, 
     And (Arc (u ":p") (valueSet [u ":a1",u ":a2"]) (Range 1 1))
         (Arc (u ":q") (valueSet [u ":b1",u ":b2"]) (Range 1 1))
     )])
  node = uri_s ":x"
  label = u "label"
  graph = Graph ts
  cs = ts
  rs = noTriples
  t1 = triple (":x", ":p", ":a1")
  t2 = triple (":x", ":q", ":b2")
  ts = Set.fromList [t1,t2]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = cs, remaining = rs, typing = singleTyping node label }
  
test_or_12_1 = validate node label ctx @?= [s]
 where 
  schema = Schema (Set.fromList [(label, 
     Or (Arc (u ":p") (valueSet [u ":a1",u ":a2"]) (Range 1 1))
        (Arc (u ":q") (valueSet [u ":b1",u ":b2"]) (Range 1 1))
     )])
  node = uri_s ":x"
  label = u "label"
  graph = Graph ts
  cs = mkset [t1]
  rs = mkset [t2]
  t1 = triple (":x", ":p", ":a1")
  t2 = triple (":x", ":r", ":c")
  ts = mkset [t1,t2]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = cs, remaining = rs, typing = singleTyping node label }

test_or_12_2 = validate node label ctx @?= [s]
 where 
  schema = Schema (Set.fromList [(label, 
     Or (Arc (u ":p") (valueSet [u ":a1",u ":a2"]) (Range 1 1))
        (Arc (u ":q") (valueSet [u ":b1",u ":b2"]) (Range 1 1))
     )])
  node = uri_s ":x"
  label = u "label"
  graph = Graph ts
  cs = mkset [t2]
  rs = mkset [t1]
  t1 = triple (":x", ":r", ":c")
  t2 = triple (":x", ":q", ":b1")
  ts = mkset [t1,t2]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = cs, remaining = rs, typing = singleTyping node label }

test_closed_or_12_12_fail = validate node label ctx @?= []
 where 
  schema = Schema (Set.fromList [(label, 
     Closed
	 (Or (Arc (u ":p") (valueSet [u ":a1",u ":a2"]) (Range 1 1))
        (Arc (u ":q") (valueSet [u ":b1",u ":b2"]) (Range 1 1))
     ))])
  node = uri_s ":x"
  label = u "label"
  graph = Graph ts
  typ = singleTyping node label
  cs = mkset [t2]
  rs = mkset [t1]
  t1 = triple (":x", ":p", ":a1")
  t2 = triple (":x", ":q", ":b1")
  ts = mkset [t1,t2]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 

test_xor_12_12_fail = validate node label ctx @?= []
 where 
  schema = Schema (Set.fromList [(label, 
	 (Xor (Arc (u ":p") (valueSet [u ":a1",u ":a2"]) (Range 1 1))
          (Arc (u ":q") (valueSet [u ":b1",u ":b2"]) (Range 1 1))
     ))])
  node = uri_s ":x"
  label = u "label"
  graph = Graph ts
  cs = mkset [t2]
  rs = mkset [t1]
  t1 = triple (":x", ":p", ":a1")
  t2 = triple (":x", ":q", ":b1")
  ts = mkset [t1,t2]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = cs, remaining = rs, typing = singleTyping node label }

test_xor_12_1 = validate node label ctx @?= [s]
 where 
  schema = Schema (Set.fromList [(label, 
	 (Xor (Arc (u ":p") (valueSet [u ":a1",u ":a2"]) (Range 1 1))
          (Arc (u ":q") (valueSet [u ":b1",u ":b2"]) (Range 1 1))
     ))])
  node = uri_s ":x"
  label = u "label"
  graph = Graph ts
  typ = singleTyping node label
  cs = mkset [t1,t2]
  rs = noTriples
  t1 = triple (":x", ":p", ":a1")
  t2 = triple (":x", ":q", ":c1")
  ts = mkset [t1,t2]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = cs, remaining = rs, typing = singleTyping node label }

test_Rec = validate node shapeA ctx @?= [s]
 where 
  schema = Schema (mkset [(shapeA, (Arc (u ":p") (ValueRef shapeA) (Range 1 1)))])
  node = uri_s ":x"
  shapeA = u "shapeA"
  graph = Graph ts
  typ = singleTyping node shapeA
  cs = mkset [t]
  rs = noTriples
  t = triple (":x", ":p", ":x")
  ts = mkset [t]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = cs, remaining = rs, typing = typ }
  
test_Ref1 = validate nodeX shapeA ctx @?= [s]
 where 
  schema = Schema ( mkset 
    [(shapeA, (Arc (u ":p") (ValueRef shapeB) (Range 1 1)))
    ,(shapeB, (Arc (u ":q") (valueSet [u ":b"]) (Range 1 1)))
	])
  nodeX = uri_s ":x"
  nodeY = uri_s ":y"
  shapeA = u "shapeA"
  shapeB = u "shapeB"
  graph = Graph ts
  typ = addType nodeY shapeB $ singleTyping nodeX shapeA
  cs = mkset [t1]
  rs = noTriples
  t1 = triple (":x", ":p", ":y")
  t2 = triple (":y", ":q", ":b")
  ts = mkset [t1,t2]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = cs, remaining = rs, typing = typ }

test_Ref2 = validate node shapeA ctx @?= [s]
 where 
  schema = Schema ( mkset 
    [(shapeA, (Arc (u ":p") (valueSet [u ":a"]) (Range 1 1)))
    ,(shapeB, (Arc (u ":q") (valueSet [u ":b"]) (Range 1 1)))
	])
  node = uri_s ":x"
  shapeA = u "shapeA"
  shapeB = u "shapeB"
  graph = Graph ts
  typ = singleTyping node shapeA
  cs = mkset [t1,t2]
  rs = noTriples
  t1 = triple (":x", ":p", ":a")
  t2 = triple (":y", ":q", ":b")
  ts = mkset [t1,t2]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = mkset [t1], remaining = noTriples, typing = typ }

test_NegRec = validate node shapeA ctx @?= []
 where 
  schema = Schema ( mkset 
    [(shapeA, (Not (Arc (u ":p") (ValueRef shapeA) (Range 1 1))))
	])
  node = uri_s ":x"
  shapeA = u "shapeA"
  graph = Graph ts
  typ = singleTyping node shapeA
  cs = mkset [t]
  rs = noTriples
  t = triple (":x", ":p", ":x")
  ts = mkset [t]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = mkset [t], remaining = noTriples, typing = typ }

test_Group_both = validate node shapeA ctx @?= [s]
 where 
  schema = Schema ( mkset 
    [(shapeA, Group (And (Arc (u ":p") (ValueType xsd_string) (Range 1 1))
	                     (Arc (u ":q") (ValueType xsd_string) (Range 1 1))) plus
	 )
	])
  node = uri_s ":x"
  shapeA = u "shapeA"
  graph = Graph ts
  typ = singleTyping node shapeA
  cs = mkset [t1]
  rs = noTriples
  t1 = tripleLit (":x", ":p", "hi")
  t2 = tripleLit (":x", ":q", "bye")
  ts = mkset [t1,t2]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = mkset [t1,t2], remaining = noTriples, typing = typ }

test_arc_string = validate node shapeA ctx @?= [s]
 where 
  schema = Schema ( mkset 
    [(shapeA, Arc (u ":p") (ValueType xsd_string) (Range 1 1))
	])
  node = uri_s ":x"
  shapeA = u "shapeA"
  graph = Graph ts
  typ = singleTyping node shapeA
  cs = mkset [t1]
  rs = noTriples
  t1 = tripleLit (":x", ":p", "hi")
  ts = mkset [t1]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = ts, remaining = noTriples, typing = typ }

test_SeveralTypes1 = validate node shapeA ctx @?= [s]
 where 
  schema = Schema ( mkset 
    [ ( shapeA, And (Arc (u ":p") (ValueRef shapeB) (From 1))
	                (Arc (u ":q") (ValueRef shapeC) (From 1))
	  )
	, ( shapeB, Arc (u ":type") (valueSet [u ":Person"]) (Range 1 1))
    , ( shapeC, Arc (u ":type") (valueSet [u ":Student"]) (Range 1 1))
	])
  node = uri_s ":a"
  shapeA = u "shapeA"
  shapeB = u "shapeB"
  shapeC = u "shapeC"
  graph = Graph ts
  typ = addType (uri_s ":s") shapeB 
      ( addType (uri_s ":t") shapeC
      ( singleTyping node shapeA
	  ))
  rs = noTriples
  t1 = triple (":a", ":p", ":s")
  t2 = triple (":a", ":q", ":t")
  ts = mkset [ t1, t2
			 , triple (":s", ":type", ":Person")
			 , triple (":t", ":type", ":Student")
			 ]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = mkset [t1,t2], 
                                  remaining = noTriples, 
								  typing = typ 
  }

test_SeveralTypes2 = validate node shapeA ctx @?= [s]
 where 
  schema = Schema ( mkset 
    [ ( shapeA, And (Arc (u ":p") (ValueRef shapeB) (From 1))
	                (Arc (u ":p") (ValueRef shapeC) (From 1))
	  )
	, ( shapeB, Arc (u ":type") (valueSet [u ":Person"]) (Range 1 1))
    , ( shapeC, Arc (u ":type") (valueSet [u ":Student"]) (Range 1 1))
	])
  node = uri_s ":a"
  shapeA = u "shapeA"
  shapeB = u "shapeB"
  shapeC = u "shapeC"
  graph = Graph ts
  typ = addTypes (uri_s ":s") [shapeB, shapeC]
      ( singleTyping node shapeA )
  rs = noTriples
  t1 = triple (":a", ":p", ":s")
  t2 = triple (":a", ":p", ":w")
  ts = mkset [ t1
             , triple (":s", ":type", ":Person")
			 , triple (":s", ":type", ":Student")
			 , t2
			 ]
  ctx = Context { schema = schema, graph = graph, currentTyping = emptyTyping } 
  s = ValidationState { checked = mkset [t1], 
                        remaining = mkset [t2], 
						typing = typ 
  }

main = defaultMain tests

tests = [ testTyping
        , testsGraph
        , testsArc
		, testsSchema
		, testsShapeArc
		, testsShapeGroup
		, testsIntegration
		]
		
testTyping = 
 testGroup "Typing" [
  testCase "combineTyping1" test_combineTyping1
 ]
 
testsGraph = 
 testGroup "Graph" [
     testCase "test_same_object" test_same_object
   , testCase "surrounding_1" test_surrounding_1
   , testCase "surrounding_2" test_surrounding_2
 ]

testsArc = 
   testGroup "ValidateArc" [
     testCase "matchArc_1" test_matchArc_1
   ]

testsShapeArc = 
 testGroup "ShapeArc" [
    testCase "testArcString" test_arc_string
  , testCase "test_arc_single" test_arc_single
  , testCase "test_arc_single_two" test_arc_single_two   
  , testCase "test_arc_12_1ok_1bad" test_arc_12_1ok_1bad
  , testCase "test_arc_12_2ok" test_arc_12_2ok
  , testCase "test_arc_2_2ok" test_arc_2_2ok
  , testCase "test_arc_12_3_fail" test_arc_12_3_fail
  , testCase "test_arc_23_1_fail" test_arc_23_1_fail
  , testCase "test_arc_11" test_arc_11
  , testCase "test_arc_11_rem" test_arc_11_rem
  ]

testsSchema =
   testGroup "Schema" [
     testCase "test_empty" test_empty
   , testCase "test_closed_empty_fail" test_closed_empty_fail 
   , testCase "test_closed_empty_succeed_empty" test_closed_empty_succeed_empty
   , testCase "test_closed_empty_succeed_no_surroounding" test_closed_empty_succeed_no_surrounding
   , testCase "test_and12" test_and_12
   , testCase "test_or_12_1" test_or_12_1
   , testCase "test_or_12_2" test_or_12_2
   , testCase "test_closed_or_12_12_fail" test_closed_or_12_12_fail
   , testCase "test_xor_12_12_fail" test_xor_12_12_fail
   , testCase "test_xor_12_1" test_xor_12_1 
   , testCase "test_Rec" test_Rec
   , testCase "test_Ref1" test_Ref1
   , testCase "test_Ref2" test_Ref2
   , testCase "test_NegRec" test_NegRec
   ]

testsShapeGroup =
   testGroup "Schema_group" [
     testCase "test_Group_both" test_Group_both 
   ]

testsIntegration =
   testGroup "Integration" [
     testCase "test_SeveralTypes1" test_SeveralTypes1 
   , testCase "test_SeveralTypes2" test_SeveralTypes2 
   ]
