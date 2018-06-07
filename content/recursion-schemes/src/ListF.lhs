> {-# OPTIONS_GHC -fno-warn-orphans  #-}
>
> {-# LANGUAGE DeriveFunctor         #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
>
> module ListF where
>
> import           Data.Functor.Classes  (Eq1(..), Show1(..), showsUnaryWith)
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

> instance Eq a => Eq1 (ListF a) where
>   liftEq _   N          N        = True
>   liftEq _  (C _ _)     N        = False
>   liftEq _   N         (C _   _) = False
>   liftEq eq (C al rl)  (C ar rr) = al == ar && eq rl rr

> instance Show1 (ListF a) where
>   liftShowsPrec _  _ _  N      = showString "N"
>   liftShowsPrec sp _ d (C _a r) = showsUnaryWith sp "C" d r -- TODO missing a
