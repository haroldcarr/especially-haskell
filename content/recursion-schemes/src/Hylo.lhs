> {-# OPTIONS_GHC -fno-warn-type-defaults      #-}
> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
>
> module Hylo where
>
> import           Ana
> import           Cata
> import           Data.List.Ordered     as O (merge)
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t, tt)
> import           TreeF

-- Recursion Patterns as Hylomorphisms
-- http://www4.di.uminho.pt/~mac/Publications/DI-PURe-031101.pdf

\textbf{hylomorphism}
---------------------

composition of catamorphism and anamorphism

- corecursive codata production followed by recursive data consumption
- can express general computation
- models *general recursion*
- enables replacing any recursive control structure with a data structure
- a representation enables exploiting parallelism

> hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
> hylo g h = cata g . ana h

> hyloL :: (a -> c -> c) -> c -> (b -> Maybe (a, b)) -> b -> c
> hyloL f z g = cataL f z . anaL' g

> -- http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=E89BCC875D547D6834D81B90093B4EC3?doi=10.1.1.51.3315&rep=rep1&type=pdf
> -- fusion/deforestation
> hyloL':: (a -> c -> c) -> c -> (b -> Maybe (a, b)) -> b -> c
> hyloL' f a g = h where
>   h b = case g b of
>     Nothing       -> a
>     Just (a', b') -> f a' (h b')

> hyloL'' :: ([b] -> b) -> (a -> [a]) -> a -> b
> hyloL'' f g = f . map (hyloL'' f g) . g

NB. termination not guaranteed

----

> fact h n0 = h c 1 a n0 where
>   a 0 = Nothing
>   a n = (Just (n, n - 1))
>   c   = (*)

> hf = U.tt "hf"
>      [ (fact hyloL  5)
>      , (fact hyloL' 5)
>      ]
>      120

----

> hl :: (Integral a) => [a] -> [(a, a)]
> hl b = hyloL f [] g b
>  where
>   g      []   = Nothing
>   g  (x:xs)   = Just ((x,x*2), xs)
>   f a@(l,_) c = if even l then a : c else c

> hle = U.t "hle"
>       (hl [1,2,3,4,5])
>       [(2,4),(4,8)]

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

----

example: Merge sort
-------------------

use a tree to capture divide / conquer pattern of recursion

- build balanced binary tree via anamorphism
- fold it with catamorphism
    - merging lists together and sorting as it goes

> mergeSort :: Ord a => [a] -> [a]
> mergeSort = hylo alg coalg where
>     alg (Leaf c)    = [c]
>     alg (Bin xs ys) = O.merge xs ys
>     coalg [x]       = Leaf x
>     coalg xs        = Bin l r where
>        (l, r)       = splitAt (length xs `div` 2) xs

note the fusion

----

> mst = U.t "mst"
>       (mergeSort [7,6,3,1,5,4,2])
>       [1,2,3,4,5,6,7]

\begin{picture}(0,0)(0,0)
\put(165,-50){\resizebox{2in}{!}{%
\begin{tikzpicture}[]
\Tree [.Bin [.Bin [.Leaf 7 ] [.Bin [.Leaf 6 ] [.Leaf 3 ] ] ] [.Bin [.Bin [.Leaf 1 ] [.Leaf 5 ] ] [.Bin [.Leaf 4 ] [.Leaf 2 ] ] ] ]
\end{tikzpicture}}}
\end{picture}

------------------------------------------------------------------------------

> testHylo :: IO Counts
> testHylo  =
>     runTestTT $ TestList $ hle ++ hf ++ mst
