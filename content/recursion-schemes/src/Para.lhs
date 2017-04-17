> module Para where

Paramorphisms
=============

*para* meaning *beside* : extension of catamorphism

- enables access to the original input structures
- provides access to input arg corresponding to running state of the recursion

> paraL :: (a -> [a] -> b -> b) -> b -> [a] -> b
> paraL f b (a : as) = f a as (paraL f b as)
> paraL _ b []       = b

> -- http://b-studios.de/blog/2016/02/21/the-hitchhikers-guide-to-morphisms/
> -- defines:
> -- para ∷    (f (μf , a)-> a)      -> μf  -> a
> -- which might be (for non-empty lists):
> -- para ∷     ([a] -> b -> b)      -> [a] -> b
> paraL':: (     [a] -> b -> b) -> b -> [a] -> b
> paraL' f b as@(_:xs) = f as (paraL' f b xs)
> paraL' _ b []        = b
