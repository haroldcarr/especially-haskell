> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
>
> module Futu where
>
> import           Ana                   (ana', takeS, s1)
> import           Control.Arrow         ((|||))
> import           CtxF
> import           Data.Functor.Foldable (Fix(..))
> import           Fixpoint
> import           StreamF
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t)

Futumorphism
============

- REF: introduced by Uustalu & Venu in 1999 [7]
- the corecursive dual of the histomorphism
- models *course-of-value* coiteration
- enables producing one or more levels

-- TODO futuL

histo : access to previously-computed values
futu  : access to values that recursion will compute in future
 http://jtobin.ca/time-traveling-recursion

> futu :: Functor f => (a -> f (Ctx f a)) -> a -> Cofix f
> futu coa = ana' ((coa ||| id) . unCtx) . hole

http://stackoverflow.com/a/36923571/814846

ListF a b = Base [a] = ConsF a b | NilF

futu :: Unfoldable t => (a -> Base  t  (Free (Base  t)  a))  -> a ->  t

ignore Unfoldable constraint
substitute [b] for t

                        (a -> Base [b] (Free (Base [b]) a))  -> a -> [b]
                        (a -> ListF b  (Free (ListF b)  a))  -> a -> [b]
                                       (Free (ListF b)  a) is a list, possibly with a hole at end
                  means it is isomorphic to ([b], Maybe a), giving:
                        (a -> ListF b       ([b], Maybe a))  -> a -> [b]
then, eliminate last ListF, since ListF a b is isomorphic to Maybe (a, b):

> -- | takes a seed value and a function that may produce at least one result
> --   and possibly a new seed value if it produced a result
> futuL ::                (a -> Maybe (b,     ([b], Maybe a))) -> a -> [b]
> futuL f a =
>   case f a of
>     Nothing            -> []
>     Just (b, (bs, ma)) -> b : (bs ++ futuBs)
>       where futuBs = case ma of
>                    Nothing -> []
>                    Just a' -> futuL f a'

Equivalent to

> notFutuL :: (a -> ([b], Maybe a)) -> a -> [b]
> notFutuL f a = case f a of
>                  (bs, ma) -> bs ++ case ma of
>                                      Nothing -> []
>                                      Just a' -> notFutuL f a'

except `futu` guarantees productivity
- if `f` returns, then not stuck waiting for next element

------------------------------------------------------------------------------
usage

> -- | deconstruct values of type Ctx f a
> unCtx :: Ctx f a -> Either a (f (Ctx f a))
> unCtx c = case unFix c of
>   Hole x -> Left x
>   Term t -> Right t

> term :: f (Fix (CtxF f a)) -> Fix (CtxF f a)
> term = Fix . Term
>
> hole :: a -> Fix (CtxF f a)
> hole = Fix . Hole

----

example: stream processing
--------------------------

pairwise exchanges elements of stream

> exch :: Stream a -> Stream a
> exch = futu coa where
>   coa xs = S (headS $ tailS xs)
>              (term $ S (headS xs)
>                        (hole $ tailS $ tailS xs))

> exs = U.t "exs"
>       (takeS 10 $ exch s1)
>       [2,1,4,3,6,5,8,7,10,9]

------------------------------------------------------------------------------

> testFutu :: IO Counts
> testFutu  =
>     runTestTT $ TestList {- $ -} exs