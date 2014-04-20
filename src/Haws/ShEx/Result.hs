-- Results as backtracking monad
module Haws.ShEx.Result where
import Haws.Monads.BackMonad
import Data.Set (Set)
import qualified Data.Set as Set

data Result a = Passed [a]
              | Failure String
 deriving (Show,Eq)

instance Monad Result where 
 return x = Passed [x]
 (>>=) = bindResult

returnResult :: a -> Result a
returnResult x = Passed [x]

bindResult :: Result a -> (a -> Result b) -> Result b
bindResult (Passed ls) f = 
 let rbs = map f ls
 in concatResult rbs
bindResult (Failure msg) f = 
 Failure msg

instance BackMonad Result where 
 orelse      = orelseResult
 failure str = Failure str
 
concatResult :: [Result b] -> Result b
concatResult = 
 foldr (\x r -> 
  case x of 
   Passed ls   -> 
    case r of 
	 Passed lsr -> Passed (ls ++ lsr)
	 Failure msg -> Passed ls
   Failure msg -> r
 ) 
 (Passed []) 

orelseResult :: Result b -> Result b -> Result b
orelseResult (Passed ls1) (Passed ls2) = Passed (ls1 ++ ls2)
orelseResult (Failure msg) (Passed ls) = Passed ls
orelseResult (Passed ls) (Failure msg) = Passed ls
orelseResult (Failure msg1) (Failure msg2) = Failure (msg1 ++ "\n" ++ msg2)

isFailure :: Result a -> Bool
isFailure (Failure _) = True
isFailure (Passed  []) = True
isFailure (Passed  _ ) = False

isPassed :: Result a -> Bool
isPassed (Passed  []) = False
isPassed (Passed  _) = True
isPassed (Failure _) = False

parts :: (Ord a) => Set a -> Result (Set a,Set a)
parts s = Passed (map setFromList2 (pSet (Set.toList s)))
 where setFromList2 :: (Ord a) => ([a],[a]) -> (Set a, Set a)
       setFromList2 (xs,ys) = (Set.fromList xs, Set.fromList ys)


-- pSet s generates the power set of s, pairing each subset
-- with its complement.
-- e.g. pSet [1,2] = [([1,2],[]),([1],[2]),([2],[1]),([],[1,2])].
-- This piece of code has been taken from: Brent Yorgey: Generating Multiset Partitions
-- http://www.haskell.org/wikiupload/d/dd/TMR-Issue8.pdf
pSet :: [a] -> [([a],[a])]
pSet [] = [([],[])]
pSet (x:xs) = mapx first ++ mapx second where
 mapx which = map (which (x:)) $ pSet xs
 first f  (x,y) = (f x, y)
 second f (x,y) = (x, f y)
