> {-# LANGUAGE DeriveFunctor #-}
>
> module CtxF where
>
> import           Data.Functor.Foldable (Fix(..))

example: Templating
-------------------

- type-safe templating requires a syntax tree with holes
- parse a string template into tree, then fill holes

Use a *free monad* structure `Ctx f a` to represent a node with
either a term of type `f` or a hole of type `a`.

> -- | A Context is a term (f r) which can contain holes a
> data CtxF f a r = Term (f r) | Hole a
>                   deriving (Functor, Show)

> -- | Context fixed-point type. A free monad.
> type Ctx f a = Fix (CtxF f a)
