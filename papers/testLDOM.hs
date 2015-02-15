module TestLDOm where

import LDOM
import Test.Framework 
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Node)
import Data.Set (Set)
import qualified Data.Set as Set
import RDF
import Sets
import Typing

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

test_empty = validate node label schema graph @?= [(cs,rs,singleTyping node label)]
 where 
  schema = Schema (Set.fromList [(label, Empty)])
  node = uri_s ":a"
  label = u "label"
  cs = noTriples
  rs = ts
  graph = Graph ts
  ts = mktriples [(":a", ":b", ":c")]
  
test_closed_empty_fail = validate node label schema graph @?= []
 where 
  schema = Schema (Set.fromList [(label, Closed Empty)])
  node = uri_s ":a"
  label = u "label"
  cs = noTriples
  rs = ts
  graph = Graph ts
  ts = mktriples [(":a", ":b", ":c")]
 
test_closed_empty_succeed_empty = validate node label schema graph @?= [(noTriples,noTriples,singleTyping node label)]
 where 
  schema = Schema (Set.fromList [(label, Closed Empty)])
  node = uri_s ":a"
  label = u "label"
  cs = noTriples
  rs = ts
  graph = Graph ts
  ts = mktriples [] 
 
test_closed_empty_succeed_no_surrounding = validate node label schema graph @?= [(noTriples,noTriples,singleTyping node label)]
 where 
  schema = Schema (Set.fromList [(label, Closed Empty)])
  node = uri_s ":a"
  label = u "label"
  cs = noTriples
  rs = ts
  graph = Graph ts
  ts = mktriples [(":b", ":p", ":c")]

test_validateArc_1 = validateArc p vo cs t rs typing @?= [(insert t cs,rs,typing)]
 where
  p = u ":p"
  vo = valueSet [u ":a1",u ":a2"]
  cs = noTriples
  t = triple (":x", ":p", ":a1")
  rs = noTriples
  typing = singleTyping (uri_s ":x") (u "label")
 
test_arc_single = validate node label schema graph @?= [(cs,rs,singleTyping node label)]
 where 
  schema = Schema (Set.fromList [(label, Arc (u ":p") (valueSet [u ":a1",u ":a2"]) plus)])
  node = uri_s ":x"
  label = u "label"
  cs = Set.fromList [t]
  rs = noTriples
  graph = Graph ts
  t = triple (":x", ":p", ":a1")
  ts = Set.fromList [t]

test_arc_single_two = validate node label schema graph @?= [(cs,rs,singleTyping node label)]
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
  
test_arc_12_1ok_1bad = validate node label schema graph @?= [(cs,rs,singleTyping node label)]
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

test_arc_12_2ok = validate node label schema graph @?= [(cs,rs,singleTyping node label)]
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

test_arc_2_2ok = validate node label schema graph @?= [(cs,rs,singleTyping node label)]
 where 
  schema = Schema (Set.fromList [(label, Arc (u ":p") (valueSet [u ":a1",u ":a2"]) (Range 2 2))])
  node = uri_s ":x"
  label = u "label"
  cs = Set.fromList [t1,t2]
  rs = Set.fromList []
  graph = Graph ts
  t1 = triple (":x", ":p", ":a1")
  t2 = triple (":x", ":p", ":a2")
  ts = Set.fromList [t1,t2]
 {- 

--
schema2 :: Schema
schema2 = Schema (set [ ("p", And (Arc (set [":a"]) (set ["1"])) 
                                  (ArcV (set [":b"]) (Var "q"))
				        )
                      , ("q", Arc (set [":c"]) (set ["2"]))
			          ])

g2 :: Graph
g2 = Graph [("x",":a","1"),("x",":b","y"),("y",":c","2")]

ctx2 = Context { typing = emptyTyping, graph = g2, schema = schema2 }

result2 = addType "y" "q" $ 
          addType "x" "p" $ emptyTyping

		  
test2 = matchSchema ctx2 "p" "x" @?= result2


test2step = showSteps $ matchSchemaStep ctx2 "p" "x" 

g21 :: Graph
g21 = Graph [("x",":a","1"),("x",":b","2")]

ctx21 = Context { typing = emptyTyping, graph = g21, schema = schema2 }
result21 = addType "y" "q" $ 
           addType "x" "p" $ emptyTyping

test21 = matchSchema ctx21 "p" "x" @?= result21
test21step = matchSchemaStep ctx21 "p" "x" 

--
schema3 :: Schema
schema3 = Schema (set [ ("p", And (Arc (set [":a"]) (set ["1"])) 
                             (Star (ArcV (set [":b"]) (Var "q")))
				)
              , ("q", And (Arc (set [":c"]) (set ["1","2"]))
			              (Arc (set [":d"]) (set ["1","2"]))
			    )
			  ])

g3 :: Graph
g3 = Graph [("x",":a","1"),("x",":b","y"),("y",":c","1"),("y",":d","2")]

ctx3 = Context { typing = emptyTyping, graph = g2, schema = schema2 }

result3 = addType "y" "q" $ addType "x" "p" $ emptyTyping

test3 = matchSchema ctx3 "p" "x" @?= result3
test3step = matchSchemaStep ctx3 "p" "x" 

-}

main = defaultMain tests

tests = 
 [ testGroup "Graph" [
     testCase "test_same_object" test_same_object
   , testCase "surrounding_1" test_surrounding_1
   , testCase "surrounding_2" test_surrounding_2
   ],
   testGroup "ValidateArc" [
     testCase "validateArc_1" test_validateArc_1
   ],
   testGroup "Schema" [
     testCase "test_empty" test_empty
   , testCase "test_closed_empty_fail" test_closed_empty_fail 
   , testCase "test_closed_empty_succeed_empty" test_closed_empty_succeed_empty
   , testCase "test_closed_empty_succeed_no_surroounding" test_closed_empty_succeed_no_surrounding
   , testCase "test_arc_single" test_arc_single
   , testCase "test_arc_single_two" test_arc_single_two   
   , testCase "test_arc_12_1ok_1bad" test_arc_12_1ok_1bad
   , testCase "test_arc_12_2ok" test_arc_12_2ok
   , testCase "test_arc_2_2ok" test_arc_2_2ok
   ]
 ]

