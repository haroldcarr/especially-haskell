> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
>
> {-# LANGUAGE MultiParamTypeClasses #-}
> module Para where
>
> import           Control.Arrow         ((&&&))
> import           Fixpoint
> import           ListF
> import           NatF
> import           Prelude               hiding (succ)
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t, tt)

Paramorphisms
=============

*para* meaning *beside* : extension of catamorphism

- models *primitive recursion* over an inductive type
- enables access to the original input structures
- operates on algebra that provides access to input arg corresponding to running state of the recursion
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

~~~{.haskell}
para :: Fixpoint f t => (f (a, t) -> a ) -> t -> a
para alg = fst . cata (alg &&& Fix . fmap snd)
~~~~

> -- more efficient
> para :: Fixpoint f t => (f (a, t) -> a) -> t -> a
> para alg = alg . fmap (para alg &&& id) . outF

-- (&&&) :: (b -> c) -> (b -> d) -> (b -> (c, d))

------------------------------------------------------------------------------

- factorial : classic example of primitive recursion
- usual `fact n = foldr (*) [1..n]` is unfold followed by fold

> fact :: Integer -> Integer
> fact = para alg where
>   alg ZeroF          = 1
>   alg (SuccF (f, n)) = f * (n + 1)

> fct = U.t "fct" (fact 10) 3628800

----

> -- factorial algebra
> falg :: Num a => NatF (a, a) -> a
> falg ZeroF          = 1
> falg (SuccF (f, n)) = f * (n + 1)

> factEqR = U.tt "factEqR"
>   [                  fact                        2
>   ,                  para falg                   2
>   , (falg .    fmap (para falg &&&   id) . outF) 2
>   , (falg .    fmap (para falg &&&   id)) (SuccF 1)
>   ,  falg   (SuccF ((para falg &&&   id)         1))
>   ,  falg   (SuccF  (para falg 1,    id          1))
>   ,                  para falg                   1              * id 1 + 1
>   ,                  para falg                   1              * 2
>   , (falg .    fmap (para falg &&&   id) . outF) 1              * 2
>   , (falg .    fmap (para falg &&&   id)) (SuccF 0)             * 2
>   ,  falg   (SuccF ((para falg &&&   id)         0))            * 2
>   ,  falg   (SuccF  (para falg 0,    id          0))            * 2
>   ,                  para falg                   0   * id 0 + 1 * 2
>   ,                  para falg                   0   *        1 * 2
>   , (falg .    fmap (para falg &&&   id) . outF) 0   *        1 * 2
>   , (falg .    fmap (para falg &&&   id)) ZeroF      *        1 * 2
>   ,  falg                                 ZeroF      *        1 * 2
>   ,                                           1      *        1 * 2
>   ]
>   2

----

> natpred :: Nat -> Nat
> natpred = para alg where
>     alg  ZeroF         = zero
>     alg (SuccF (_, n)) = n

> np = U.t "np"
>      (natpred (succ (succ (succ zero))))
>                     (succ (succ zero))

> tailL :: List a -> List a
> tailL = para alg where
>     alg  N           = nil
>     alg (C _ (_, l)) = l

> tl = U.t "tl"
>      (tailL (cons 1 (cons (2::Int) nil)))
>                     (cons  2       nil)

----

example: sliding window
-----------------------

> sliding :: Int -> [a] -> [[a]]
> sliding n = para alg where
>   alg N             = []
>   alg (C x (r, xs)) = take n (x:xs) : r

NB. lookahead via input arg is left-to-right, but input list processed from the right

> sl = U.t "sl"
>      (sliding 3 [1..5::Int])
>      [[1,2,3],[2,3,4],[3,4,5],[4,5],[5]]

example: slideing window 2

> sliding2 :: Int -> [a] -> [[a]]
> sliding2 n = para alg where
>   alg N             = []
>   alg (C x (r, xs)) | length (x:xs) < n = []
>                     | otherwise         = take n (x:xs) : r

NB. lookahead via input arg is left-to-right, but input list processed from the right

> sl2 = U.t "sl2"
>      (sliding2 3 [1..5::Int])
>      [[1,2,3],[2,3,4],[3,4,5]]

 http://stackoverflow.com/a/13317563/814846

------------------------------------------------------------------------------

> testPara :: IO Counts
> testPara  =
>     runTestTT $ TestList $ fct ++ infz ++ infs ++ otfz ++ otfs ++ factEqR ++ np ++ tl ++ sl ++ sl2
