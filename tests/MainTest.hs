module Main (
    main
 ) where
 
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
 
import Haws.TestFunTGraph
import Haws.TestFGLTGraph
import Haws.TestTGraph
 
main :: IO ()
main = defaultMain Main.tests
 
tests :: [Test]
tests = Haws.TestFunTGraph.tests ++
        Haws.TestFGLTGraph.tests ++
        Haws.TestTGraph.tests
        
