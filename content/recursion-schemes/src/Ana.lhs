> {-# LANGUAGE ViewPatterns #-}
>
> module Ana where
>
> import Data.Functor.Foldable (Fix(..))

Anamorphisms
============

*ana* meaning *upwards* : generalized unfold

- corecursive dual of catamorphisms
- co-inductive co-recursion : each recursive step guarded by a constructor
- produces streams and other regular structures from a seed
- `ana` for lists is `unfoldr` (`ViewPatterns` help see the duality)
- although result can be infinite, each step (a constructor) is produced in finite time (i.e., makes progress)


> anaL  :: (b ->       (a, b))               -> b -> [a]
> anaL  f b = let (a, b') = f b in a : anaL f b'

> anaL' :: (b -> Maybe (a, b))               -> b -> [a]
> anaL' f b = case f b of
>   Just (a, b') -> a : anaL' f b'
>   Nothing      -> []

~~~{.haskell}
foldrP  :: (Maybe (a, b) -> b) -> [a] -> b
foldrP f []     = f Nothing
foldrP f (x:xs) = f (Just (x, foldrP f xs))
~~~

> unfoldr :: (b -> Maybe (a, b)) ->  b -> [a]
> unfoldr f (f -> Nothing)                   = []
> unfoldr f (f -> Just (x, unfoldr f -> xs)) = x : xs
> unfoldr _ _ = error "unfoldr"

No checked distinction between data and codata in Haskell

- so use of `Fix` again

> -- | anamorphism
> ana :: Functor f => (a -> f a) -> a -> Fix f
> ana coalg = Fix . fmap (ana coalg) . coalg

to distinquish data/codata (useful when working with streams)

> newtype Cofix f = Cofix { unCofix :: f (Cofix f) }

compare to

~~~{.haskell}
newtype Fix   f = Fix   { unFix   :: f (Fix   f) }
~~~

use in `ana` definition

> -- | an alternative anamorphism typed for codata
> ana' :: Functor f => (a -> f a) -> a -> Cofix f
> ana' coalg = Cofix . fmap (ana' coalg) . coalg
