% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[BT] {Backtracking monad transformer}

These definitions have been derived from \cite{Hinze00b}.

\begin{comment}
\begin{code}

module Haws.Monads.BT where
import Haws.Monads.MonadT
import Haws.Monads.BackMonad


\end{code}
\end{comment}

It changes a monad to obtain a new monad with backtracking

\begin{code}

newtype BT m a 
    = BT (forall b. ((a -> m b -> m b) -> m b -> m b))

unBT (BT x) = x

instance Monad m => Monad (BT m) where
        return x = BT (\c -> c x)
        m >>= f = 
            BT (\c -> (unBT m) (\a -> unBT (f a) c))

instance (Monad m, Monad (BT m))
               => MonadT BT m where
   lift m = BT (\k f -> do { x <- m
                           ; k x f
                           })

\end{code}

|BT| turns a monad into a backtracking monad (|BackMonad|), 
with the operators |failure| and |orelse|. 
|failure| fails the current computation. 
|orelse m n| tries |m| and if it fails, tries |n|

\begin{code}

instance (Monad m) => 
            BackMonad (BT m) where
 failure 
     = BT (\k -> id)

 m `orelse` n 
     = BT (\k f -> (unBT m) k ((unBT n) k f))


lowerBT :: Monad m => BT m a -> m a
lowerBT (BT f) = f (\a k -> return a) (fail "false")

\end{code}


