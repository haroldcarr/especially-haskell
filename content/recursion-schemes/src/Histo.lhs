> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults      #-}
>
> {-# LANGUAGE ViewPatterns #-}
>
> module Histo where
>
> import           AnnF
> import           Cata                  (cata, cataL)
> import           Control.Arrow         ((&&&))
> import           Fixpoint
> import           ListF
> import           NatF
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t, tt)

\textbf{histomorphism}
----------------------

- REF: introduced by Uustalu & Venu in 1999 [7]
- models *course-of-value recursion* : gives access to previously computed values
    - moves bottom-up annotating tree with results
    - then collapses tree producing result
- useful for applying dynamic programming techniques to recursive structures

> -- | Histomorphism
> histo :: Fixpoint f t => (f (Ann f a) -> a) -> t -> a
> histo alg = attr . cata (ann . (id &&& alg))

> -- TODO: is this useful?
> -- http://stackoverflow.com/a/24892711/814846
> -- gives access to all previous values
> histoL' ::     ([a]      -> a)      -> [a] -> a
> histoL' f = head . go where
>   go [] = [f []]
>   go as = let histvals = go as in f histvals : histvals

----------
from my SO question

> -- | list of
> -- Step pairs: a : input, b: result
> -- Zero : the zero argument given to histo
> -- Pair each step/layer of input with its result.
> data History a b
>   = Zero   b
>   | Step a b (History a b)
>   deriving (Eq, Read, Show)

> -- folds from right to left
> history :: (a -> History a b -> b) -> b -> [a] -> History a b
> history f b = cataL (\a h -> Step a (f a h) h) (Zero b)

> headH :: History a b -> b
> headH (Zero   b)   = b
> headH (Step _ b _) = b

> prevH :: History a b -> History a b
> prevH (Step _ _ h) = h
> prevH z            = z

> -- final result at top of stack
> histoL :: (a -> History a b -> b) -> b -> [a] -> b
> histoL f b = headH . history f b

------------------------------------------------------------------------------

> thistory = U.t "thistory"
>   (history (\a _ -> (show a)) "" [1,2,3])
>   (Step 1 "1" (Step 2 "2" (Step 3 "3" (Zero ""))))

example: computing Fibonacci numbers
------------------------------------

> fibHL :: Integer -> History Integer Integer
> fibHL n0 = history f 1 [3..n0] where
>   f _ h = headH h + headH (prevH h)

> tfibHL = U.t "tfibHL"
>        (fibHL 8)
>        (Step 3 21 (Step 4 13 (Step 5 8 (Step 6 5 (Step 7 3 (Step 8 2 (Zero 1)))))))

> fibL :: Integer -> Integer
> fibL = headH . fibHL

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

> fib100 = U.tt "fib100"
>        [ fib  100
>        , fibL 100
>        ]
>        354224848179261915075

> tfib = U.t "tfib"
>        (map (\x -> (fibL x, fib x)) [2..10])
>        [(1,1),(2,2),(3,3),(5,5),(8,8),(13,13),(21,21),(34,34),(55,55)]

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
>      (evens [1..7])
>      [2,4,6]

------------------------------------------------------------------------------

> testHisto :: IO Counts
> testHisto  =
>     runTestTT $ TestList $ thistory ++ tfibHL ++ fib100 ++ tfib ++ ev
