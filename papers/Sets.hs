module Sets where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List(intersperse)


-- Common definition on sets	
set :: (Ord a) => [a] -> Set a
set = Set.fromList

elems = Set.elems

showSet :: Show a => Set a -> String
showSet s = "{" ++ concat (intersperse "," (Set.toList (Set.map show s))) ++ "}"

insert :: Ord a => a -> Set a -> Set a
insert = Set.insert 

isEmpty = Set.null

member :: (Ord a) => a -> Set a -> Bool
member = Set.member

divide :: (Ord a) => Set a -> (a, Set a)
divide ts = 
 if Set.null ts then error "divide applied to an empty set"
 else Set.deleteFindMax ts
