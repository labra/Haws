% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[BackMonad] {Backtracking monad}

\begin{comment}
\begin{code}
module Haws.Monads.BackMonad where
\end{code}
\end{comment}

\begin{code}

class Monad m => BackMonad m where
   failure :: String -> m a
   orelse  :: m a -> m a -> m a

guard ::(BackMonad m
        ) => Bool -> m ()
guard b = if b then return ()
          else failure "guard failed"

\end{code}

