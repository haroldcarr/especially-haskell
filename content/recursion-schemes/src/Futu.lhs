> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
>
> module Futu where
>
> import           Ana                   (ana', sFrom1, sFrom1L, takeS)
> import           Control.Arrow         ((|||))
> import           CtxF
> import           Data.Functor.Foldable (Fix(..))
> import           Fixpoint
> import           StreamF
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (tt)

\textbf{futumorphism}
---------------------

- REF: introduced by Uustalu & Venu in 1999 [7]
- corecursive dual of histomorphism
    - histo : access to previously-computed values
    - futu  : access to values that recursion will compute in future
    http://jtobin.ca/time-traveling-recursion
- models *course-of-value* coiteration
- enables producing one or more levels

-- TODO futuL


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
>               Nothing -> []
>               Just a' -> futuL f a'

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

> exchL = futuL coa where
>   coa xs = Just ( head (tail xs),
>                   ( [head xs],
>                     Just (tail (tail xs))
>                   )
>                 )

> exs1 = U.tt "exs1"
>        [ takeS 10 $ exch  sFrom1
>        , take  10 $ exchL sFrom1L
>        ]
>        [2,1,4,3,6,5,8,7,10,9]

> exs2 = U.tt "exs2"
>        [ takeS  9 $ exch  sFrom1
>        , take   9 $ exchL sFrom1L
>        ]
>        [2,1,4,3,6,5,8,7,10]

> exs3 = U.tt "exs3"
>        [ takeS  7 $ exch  sFrom1
>        , take   7 $ exchL sFrom1L
>        ]
>        [2,1,4,3,6,5,8]

------------------------------------------------------------------------------

> testFutu :: IO Counts
> testFutu  =
>     runTestTT $ TestList $ exs1 ++ exs2 ++ exs3
