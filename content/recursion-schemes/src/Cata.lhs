> module Cata where
>
> import Fixpoint

Catamorphisms
=============

*cata* meaning *downwards* : generalized fold

- models (internal) *iteration*
- inductive recursion : each recursive step consumes one or more constructors
- ensures terminates (if given finite input)

----

~~~{.haskell}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)
~~~

> cataL :: (a ->        b -> b) -> b -> [a] -> b
> cataL f b (a : as) = f a    (cataL f b as)
> cataL _ b      []  = b

~~~{.haskell}
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
~~~

> cata :: Fixpoint f t => (f a -> a) -> t -> a
> cata alg = alg . fmap (cata alg) . outF
