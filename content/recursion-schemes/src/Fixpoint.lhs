> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
>
> module Fixpoint where
>
> import           Data.Functor.Foldable (Fix(..))
>

> class Functor f => Fixpoint f t | t -> f where
>   inF  :: f t -> t
>   outF :: t -> f t

~~~{.haskell}
newtype Fix f = Fix { unFix :: f (Fix f) }

fix          :: f -> Fix f
fix           = Fix
~~~

> -- think of `unFix` as
> unFix        :: Fix f -> f (Fix f)
> unFix (Fix f) = f

> instance Functor f => Fixpoint f (Fix f) where
>   inF  = Fix
>   outF = unFix
