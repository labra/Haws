% 
% (c) Jose E. Labra . University of Oviedo (2000)
%
\section[Laws] {Laws}

\begin{comment}
\begin{code}
module Haws.Monads.Laws 
   ( Law
   , (===)
   ) where
\end{code}
\end{comment}

A way to write some laws in Haskell source code 
taken from \cite{JonesDuponcheel93}

\begin{code}
infix 1 ===

data Law a = Unspecified

(===) :: a -> a -> Law a
x === y = error "uncomputable law equality"

\end{code}