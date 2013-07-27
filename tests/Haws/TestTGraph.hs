module Haws.TestTGraph(tests)
where

import Test.Framework 
import Test.QuickCheck
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 

import Haws.TGraph
import Haws.FunTGraph 
import Haws.TContext
import Data.Set
import Data.Maybe
import Prelude hiding(pred,succ)

main = defaultMain tests

tests = [
        testGroup "Empty TGraph" [
                testCase "empty" test_empty,
                testCase "empty nodes" test_emptyNodes,
                testCase "get nodes" test_getNodes,
                testCase "decomp a" test_decomp_a,
                testCase "decomp b" test_decomp_b,
                testCase "decomp p" test_decomp_p
            ]
    ]


test_empty = 
 let e :: FunTGraph Int
     e = gEmpty
 in (isEmpty e) @?= True

test_emptyNodes = 
   let e :: FunTGraph Int
       e = gEmpty
   in (nodes e) @?= Data.Set.empty
   
test_getNodes =
   let e :: FunTGraph Char
       e = gEmpty 
       mb1 :: FunTGraph Char
       mb1 = insertTriples (fromList 
          [('a','p','b'),
           ('b','q','a'),
           ('b','r','c'),
           ('c','s','a')])
           e
   in (nodes mb1) @?= fromList ['a','b','c','p','q','r','s']

test_decomp_a =
   let e :: FunTGraph Char
       e = gEmpty 
       g :: FunTGraph Char
       g = insertTriples (fromList 
          [('a','p','b'),
           ('b','q','a'),
           ('b','r','c'),
           ('c','s','a')])
           e
       rest = insertTriples (fromList [('b','r','c')]) e
       
   in fst (fromJust (decomp 'a' g)) @?= 
         Ctx { node = 'a', 
               pred = fromList [('b','q'),('c','s')],
               succ = fromList [('p','b')],
               rels = fromList []
             }
              
              
test_decomp_b =
   let e :: FunTGraph Char
       e = gEmpty 
       g :: FunTGraph Char
       g = insertTriples (fromList 
          [('a','p','b'),
           ('b','q','a'),
           ('b','r','c'),
           ('c','s','a')])
           e
       rest = insertTriples (fromList [('c','s','a')]) e
       
   in fst (fromJust (decomp 'b' g)) @?= 
         Ctx { node = 'b', 
               pred = fromList [('a','p')],
               succ = fromList [('q','a'),('r','c')],
               rels = fromList []
             }                                

test_decomp_p =
   let e :: FunTGraph Char
       e = gEmpty 
       g :: FunTGraph Char
       g = insertTriples (fromList 
          [('a','p','b'),
           ('b','q','a'),
           ('b','r','c'),
           ('c','s','a')])
           e
       rest = insertTriples (fromList [('c','s','a')]) e
       
   in fst (fromJust (decomp 'p' g)) @?= 
         Ctx { node = 'p', 
               pred = fromList [],
               succ = fromList [],
               rels = fromList [('a','b')]
             }                                
             