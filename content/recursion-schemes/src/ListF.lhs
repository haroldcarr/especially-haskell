> {-# OPTIONS_GHC -fno-warn-orphans  #-}
>
> {-# LANGUAGE DeriveFunctor         #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
>
> module ListF where
>
> import           Data.Functor.Foldable (Fix(..))
> import           Fixpoint

> data ListF a r = C a r | N deriving (Eq, Functor, Show)

> type List a    = Fix (ListF a)

> nil           :: List a
> nil            = Fix N
> cons          :: a -> List a -> List a
> cons x xs      = Fix (C x xs)

> instance Fixpoint (ListF a) [a] where
>   inF N        = []
>   inF (C x xs) = x : xs
>   outF []      = N
>   outF (x:xs)  = C x xs
