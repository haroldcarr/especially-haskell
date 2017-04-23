> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE ViewPatterns  #-}
>
> module AnnF where
>
> import           Cata                  (cata)
> import           Data.Functor.Foldable (Fix(..))
> import           Fixpoint

example: Annotating
-------------------

- useful for storing intermediate values
- inspired by ideas from *attribute grammars*

Use a *cofree comonad* structure `Ann f a` to annotate nodes of
type `f` with attributes of type `a`.

> -- | Annotate (f r) with attribute a
> newtype AnnF f a r = AnnF (f r, a) deriving Functor

> -- | Annotated fixed-point type. A cofree comonad.
> type Ann f a = Fix (AnnF f a)

> -- | annotation constructor
> ann :: (f (Ann f a), a) -> Ann f a
> ann = Fix . AnnF

> -- | annotation deconstructor
> unAnn :: Ann f a -> (f (Ann f a), a)
> unAnn (unFix -> AnnF a) = a

> -- | Attribute of the root node
> attr :: Ann f a -> a
> attr (unFix -> AnnF (_, a)) = a

> -- | strip attribute from root
> strip :: Ann f a -> f (Ann f a)
> strip (unFix -> AnnF (x, _)) = x

> -- | strip all attributes
> stripAll :: Functor f => Ann f a -> Fix f
> stripAll = cata alg where
>   alg (AnnF (x, _)) = Fix x

