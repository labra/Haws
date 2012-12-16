module Haws.TestFGLTGraph(tests)
where

import Test.Framework 
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 

import Test.QuickCheck
import Test.HUnit

import Haws.FGLTGraph
import Haws.TGraph
import Data.Set

main = defaultMain tests

tests = [
        testGroup "Empty FGLTGraph" [
                testCase "empty" test_empty,
                testCase "empty nodes" test_emptyNodes
            ]
    ]


test_empty = 
   let e :: FGLTGraph Int
       e = gEmpty
   in (isEmpty e) @?= True

test_emptyNodes = 
   let e :: FGLTGraph Int
       e = gEmpty
   in (nodes e) @?= Data.Set.empty

