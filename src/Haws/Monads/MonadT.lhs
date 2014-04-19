% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[MonadT] {Monad Transformer}

\begin{comment}
\begin{code}
module Haws.Monads.MonadT 
    (MonadT(..)
    ) where
    
import Haws.Monads.Laws

\end{code}
\end{comment}

A monad transformer is a type operation that transforms one monad into
another. 
A monad transformer |t| provides a function that lifts computations in |m|
to computations in |t m|.

\begin{code}
class (Monad m, 
       Monad (t m)) => MonadT t m where
 lift :: m a -> t m a

\end{code}

|lift| must be a \emph{monad morphism} \cite{Moggi89}. 

A monad morphism is a function between monads |m| and |m'| 
of type |f:: m a -> m' a| satisfying:

\begin{itemize}
\item |f . return| = |return|.
\item |f (m >>= k)| = |f m >>= (f . k)|.
\end{itemize}

In particular, if |f| is |lift| we have:

\begin{code}

law1     = lift . return  
       === return

law2 m k = lift (m >>= k) 
       === (lift m >>= lift . k)


law1 :: ( MonadT t m) => Law (a -> t m a)
law2 :: ( MonadT t m
        ) => m a -> (a -> m b) -> Law (t m b)

\end{code}

Any monad is also a functor, so we can give a default definition:

\begin{code}
{-instance Monad m => Functor m where
  fmap f m = do { x <- m 
                ; return (f x) 
                }-}
\end{code}

