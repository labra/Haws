module Haws.ShEx.Context where
import Haws.ShEx.RDFModel
import Haws.ShEx.Shape
import Haws.ShEx.Typing

data Context = Context { 
   graph  :: RDFGraph ,
   shEx   :: ShEx,
   typing :: Typing      
}
