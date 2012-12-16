module Main (
    main
 ) where
 
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
 
import Haws.TestTGraph
import Haws.TestFGLTGraph
 
main :: IO ()
main = defaultMain Main.tests
 
tests :: [Test]
tests = Haws.TestTGraph.tests ++
        Haws.TestFGLTGraph.tests
        
