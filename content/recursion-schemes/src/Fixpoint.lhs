> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
>
> module Fixpoint where
>
> import           Data.Functor.Foldable (Fix(..))
>

factor recursion out of data types with `Fix`
=============================================

- benefits:
    - reason about recursion and base structures separately
- applicable to types that have recursive structure
    - e.g., lists, trees

`Fix` definition

~~~{.haskell}
newtype Fix f = Fix { unFix :: f (Fix f) }

fix          :: f -> Fix f
fix           = Fix
~~~

> -- think of `unFix` as
> unFix        :: Fix f -> f (Fix f)
> unFix (Fix f) = f

- `fix` adds one level of recursion
- `unfix` removes one level of recursion

`Fix` *is "generic" recursive structure*

- write recursive type without using recursion
- use `Fix` to add recursion

Working with fixed data-types
=============================

a type class (using `FunctionalDependencies` [FD]) to
(transparently) apply isomorphism between (un)fixed representations

> class Functor f => Fixpoint f t | t -> f where
>   inF  :: f t -> t
>   outF :: t -> f t

> instance Functor f => Fixpoint f (Fix f) where
>   inF  = Fix
>   outF = unFix

To distinquish data/codata (useful when working with streams)

> newtype Cofix f = Cofix { unCofix :: f (Cofix f) }

compare to

~~~{.haskell}
newtype Fix   f = Fix   { unFix   :: f (Fix   f) }
~~~

use in `ana` definition

