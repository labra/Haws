module Haws.TestFunTGraph(tests)
where

import Test.Framework 
import Test.QuickCheck
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 

import Haws.FunTGraph
import Haws.TGraph
import Data.Set

main = defaultMain tests

tests = [
        testGroup "Empty FunTGraph" [
                testCase "empty" test_empty,
                testCase "empty nodes" test_emptyNodes
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
