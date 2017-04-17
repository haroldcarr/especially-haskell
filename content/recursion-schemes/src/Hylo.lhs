> module Hylo where
>
> import Ana
> import Cata

-- Recursion Patterns as Hylomorphisms
-- http://www4.di.uminho.pt/~mac/Publications/DI-PURe-031101.pdf

Hylomorphism
============

composition of catamorphism and anamorphism

- corecursive codata production followed by recursive data consumption
- can express general computation
- models *general recursion*
- enables replacing any recursive control structure with a data structure
- a representation enables exploiting parallelism

> hyloL :: (a -> c -> c) -> c -> (b -> Maybe (a, b)) -> b -> c
> hyloL f z g = cataL f z . anaL' g

> -- fusion/deforestation
> hyloL':: (a -> c -> c) -> c -> (c -> Maybe (a, c)) -> c
> hyloL' f z g = case g z of
>   Nothing     -> z
>   Just (x,z') -> f x (hyloL' f z' g)

> hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
> hylo g h = cata g . ana h

NB. termination not guaranteed

----

`cata` and `ana` can be fused

- via substitution and fmap-fusion Functor law

~~~{.haskell}
fmap p . fmap q = fmap (p . q)
~~~

giving

~~~{.haskell}
hylo f g = f . fmap (hylo f g) . g
~~~

means not necessary to build full structure for `cata` and `ana`

- basis for *deforestation*
    - eliminating intermediate data structures

side note : `cata` and `ana` could be defined as

~~~{.haskell}
cata f = hylo f unFix
ana  g = hylo Fix g
~~~~
