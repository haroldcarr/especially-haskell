> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE ViewPatterns  #-}
>
> module StreamF where
>
> import Fixpoint

example: coinductive streams
----------------------------

> -- like `List` but no base case (i.e., Nil)
> data StreamF a r = S a r deriving (Functor, Show)
> type Stream a = Cofix (StreamF a)

~~~{.haskell}
-- derived
instance Functor (StreamF a) where
  fmap f (S x xs) = S x (f xs)
~~~

constructor/deconstructors:

> consS :: a -> Cofix (StreamF a) -> Cofix (StreamF a)
> consS x xs = Cofix (S x xs)

> headS :: Cofix (StreamF a) -> a
> headS (unCofix -> (S x _ )) = x

> tailS :: Cofix (StreamF a) -> Cofix (StreamF a)
> tailS (unCofix -> (S _ xs)) = xs
