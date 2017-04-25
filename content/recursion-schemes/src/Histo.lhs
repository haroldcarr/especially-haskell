> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults      #-}
>
> {-# LANGUAGE ViewPatterns #-}
>
> module Histo where
>
> import           AnnF
> import           Cata                  (cata)
> import           Control.Arrow         ((&&&))
> import           Fixpoint
> import           ListF
> import           NatF
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t)

Histomorphism
=============

- REF: introduced by Uustalu & Venu in 1999 [7]
- models *course-of-value recursion* : enables using arbitrary previously computed values
- useful for applying dynamic programming techniques to recursive structures

moves bottom-up annotating tree with results
then collapses tree producing result

> -- | Histomorphism
> histo :: Fixpoint f t => (f (Ann f a) -> a) -> t -> a
> histo alg = attr . cata (ann . (id &&& alg))

> -- http://stackoverflow.com/a/24892711/814846
> -- gives access to all previous values
> histoL' ::     ([a]      -> a)      -> [a] -> a
> histoL' f = head . go where
>   go [] = [f []]
>   go as = let histvals = go as in f histvals : histvals

> fibL' :: [Integer] -> Integer
> fibL' = histoL' $ \a -> case a of
>   (x:y:_) -> x + y
>   _       -> 1

----------
from my SO

> -- | list of pairs of things and results
> -- with extra result at end corresponding to the []-thing.
> -- Used to paireach layer of input list with its corresponding result.
> data History a b = Ancient b | Age a b (History a b)

> cataL = foldr

> history :: (a -> History a b -> b) -> b -> [a] -> History a b
> history f z = cataL (\x h -> Age x (f x h) h) (Ancient z)

-- After folding list from right to left, final result is at top of stack.

> headH :: History a b -> b
> headH (Ancient x) = x
> headH (Age _ x _) = x

> histoL :: (a -> History a b -> b) -> b -> [a] -> b
> histoL f z = headH . history f z

------------------------------------------------------------------------------

example: computing Fibonacci numbers
------------------------------------

> fib :: Integer -> Integer
> fib = histo f where
>   f :: NatF (Ann NatF Integer) -> Integer
>   f ZeroF                                         = 0
>   f (SuccF (unAnn -> (ZeroF                 ,_))) = 1
>   f (SuccF (unAnn -> (SuccF (unAnn -> (_,n)),m))) = m + n
>   f _    = error "fib"

$$
\begin{array}{rcl}
F_0 & = & 0 \\
F_1 & = & 1 \\
F_n & = & F_{n-1} + F_{n-2} \\
\end{array}
$$

> fibt = U.t "fibt"
>        (fib 100)
>        354224848179261915075

----

example: filtering by position
------------------------------

The function `evens` takes every second element from the given list.

> evens :: [a] -> [a]
> evens = histo alg where
>   alg N                      = []
>   alg (C _ (strip -> N    )) = []
>   alg (C _ (strip -> C x y)) = x : attr y
>   alg _                      = error "evens"

> ev = U.t "ev"
>      (evens [1..7::Int])
>      [2,4,6]

------------------------------------------------------------------------------

> testHisto :: IO Counts
> testHisto  =
>     runTestTT $ TestList $ fibt ++ ev
