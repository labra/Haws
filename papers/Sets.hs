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

delete :: Ord a => a -> Set a -> Set a
delete = Set.delete

isEmpty = Set.null

member :: (Ord a) => a -> Set a -> Bool
member = Set.member

mkset :: (Ord a) => [a] -> Set a
mkset = Set.fromList 

divide :: (Ord a) => Set a -> (a, Set a)
divide ts = 
 if Set.null ts then error "divide applied to an empty set"
 else Set.deleteFindMax ts

decomp :: (Ord a) => Set a -> [(a, Set a)]
decomp s = map (\x -> (x, Set.delete x s)) $ 
           elems s
		   
decompBy :: (Ord a) => (a -> Bool) -> Set a -> [(a,Set a)]
decompBy c s = map (\x -> (x, Set.delete x s)) $ 
               filter c $
               elems s
