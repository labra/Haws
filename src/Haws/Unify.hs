-- | The following code has been adapted from:
-- "Polytipic Unification" (P. Jansson, J. Jeuring)
-- J. Functional Programming

module Haws.Unify
    ( Children(..)
    , VarCheck(..)
    , TopEq(..)
    , Term(..)
    , Subst(..)
    , appSubst
    , unify
    , unify'
    ) where

class Children t where 
 children :: t -> [t]
 mapChildren :: (t -> t) -> t -> t

type VarName    = String      

class VarCheck t where
 varCheck :: t -> Maybe VarName
 
class TopEq t where                   
 topEq :: t -> t -> Bool


class ( Children t
      , VarCheck t
      , TopEq t) => Term t

class Subst s where
 idSubst :: (Term t) => s t
 modBind :: (Term t) => (VarName,t) -> s t -> s t
 lookupin :: (Term t) => s t -> VarName -> Maybe t

------------------------------------------------
appSubst::( Subst s
          , Term t
          ) => s t -> t -> t
appSubst s t = 
    case varCheck t of
     Nothing -> mapChildren (appSubst s) t
     Just v  -> case lookupin s v of
                 Nothing -> t
                 Just t' -> appSubst s t'

------------------------------------------------
unify ::( Term t
        , Subst s
        ) => t -> t -> Maybe (s t)

unify'::( Term t
        , Subst s 
        ) => t -> t -> s t -> Maybe (s t)

unify tx ty = unify' tx ty idSubst
unify' tx ty = uni (varCheck tx,varCheck ty) 
 where
  uni (Nothing,Nothing) | topEq tx ty = uniTerms tx ty
                        | otherwise   = dontUnify
  uni (Just i, Just j)  | i == j      = okUnify
  uni (Just i, _     )  = i ||-> ty
  uni (_     , Just j)  = j ||-> tx
  
  uniTerms x y = threadList (zipWith unify' (children x)
                                            (children y))

(||->)::
    ( Term t
    , Subst s
    ) => VarName -> t -> s t -> Maybe (s t)
(i ||-> t) s = 
    if occursCheck i s t then dontUnify s
    else case lookupin s i of
          Nothing -> okUnify (modBind (i,t) s)
          Just t' -> mapMaybe (modBind (i,t)) 
                              (unify' t t' s)

---------------------------------------------------------
-- Auxiliary functions

vars :: Term t => t -> [VarName]
vars t = [v | Just v <- map varCheck (subTerms t)]

subTerms :: Children t => t -> [t]
subTerms t = t : concat (map subTerms (children t))

occursCheck ::( Term t
              , Subst s
              ) => VarName -> s t -> t -> Bool
occursCheck i s t = i `elem` reachLs s (vars t)
 where
  reachLs s l = l ++ concat (map (reachable s) l)
  reachable s v = reachLs s (maybe [] vars (lookupin s v))

threadList :: Monad m => [a -> m a] -> a -> m a
threadList = foldr (@@>) return

(@@>) :: Monad m => (a -> m b) -> (c -> m a) -> (c -> m b)
(f @@> g) x = g x >>= f

mapMaybe f = maybe Nothing (Just . f)

okUnify,dontUnify :: a -> Maybe a
dontUnify = const Nothing
okUnify  = Just


-------------------------------------------------
-- A simple example
-------------------------------------------------------
type Name = String

data T = V VarName
     | F Name [T]
     | C Name
 deriving Show

instance Children T where
 children (V v) = []
 children (F n ts ) = ts
 children (C _) = []

 mapChildren f (V v) = V v
 mapChildren f (F n ts) = F n (map f ts)
 mapChildren f (C n) = C n

instance VarCheck T where
 varCheck (V v) = Just v
 varCheck _     = Nothing

instance TopEq T where
 topEq (F n l) (F n' l') = n == n'
 topEq (V v)   (V v')    = v == v'
 topEq (C n)   (C n')    = n == n'
 topEq _     _           = False

instance Term T

newtype MSubst t = Subs [(VarName,t)]
 deriving Show

instance Subst MSubst where
 idSubst = Subs []
 modBind (x,t) (Subs ts) = Subs ((x,t):ts)
 lookupin (Subs ts) x = lookup x ts

-- Examples
 
x = V "x"
x' = V "x'"
y = V "y"
a = C "a"
b = C "b"

pxa = F "p" [x,a]
pbx = F "p" [b,x]
pby = F "p" [b,y]

pxx = F "p" [x,x]
pyfy = F "p" [y, F "f" [y]]

test :: T -> T -> Maybe (MSubst T)
test = unify

run :: [(T,T)] -> Maybe (MSubst T)
run ts = foldr (\(t1,t2) m -> 
                    case m of
                     Nothing -> Nothing
                     Just s  -> unify' t1 t2 s)
               (Just idSubst)
               ts
 