> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
>
> {-# LANGUAGE RankNTypes    #-}
> {-# LANGUAGE ViewPatterns  #-}
>
> module Ana where
>
> import           Control.Arrow         (second)
> import           Data.Functor.Foldable (Fix(..))
> import           Fixpoint
> import           NatF
> import           Prelude hiding        (replicate)
> import           StreamF
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t, tt)

------------------------------------------------------------------------------
definition

\textbf{anamorphism}
---------------------

*ana* meaning *upwards* : generalized unfold

- corecursive dual of catamorphisms / recursion
- recursion consumes (necessarily finite) *data*
- corecursion produces (potentially infinite) *codata*
- produces streams and other regular structures from a seed
- co-inductive co-recursion : each recursive step guarded by a constructor
- although result can be infinite, each step (a constructor) is produced in finite time (i.e., makes progress)


> anaL  :: (b ->       (a, b))               -> b -> [a]
> anaL  f b = let (a, b') = f b in a:anaL f b'

> anaL' :: (b -> Maybe (a, b))               -> b -> [a]
> anaL' f b = case f b of
>               Just (a, b') ->   a:anaL' f b'
>               Nothing      -> []

- `ana` for lists is `unfoldr` (`ViewPatterns` help see the duality)

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

compare to

~~~{.haskell}
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg  = alg . fmap (cata alg)  . unFix
~~~

-- rewrite using Cofix

> -- | an alternative anamorphism typed for codata
> ana' :: Functor f => (a -> f a) -> a -> Cofix f
> ana' coalg = Cofix . fmap (ana' coalg) . coalg

------------------------------------------------------------------------------
usage

> replicate :: Int -> a -> [a]
> replicate n0 x = anaL' c n0 where
>   c 0 = Nothing
>   c n = Just (x, n-1)

> rep = U.t "rep" (replicate 4 '*') "****"

-------------------------

> fibs :: [Integer]
> fibs = anaL (\(a, b) -> (a, (b, a + b)))
>             (0, 1)

> fib = U.t "fib" (fibs !! 7) 13

> fibs' :: [Integer]
> fibs' = anaL' (\(a, b) -> Just (a, (b, a + b)))
>              (0, 1)

> fib' = U.t "fib'" (fibs' !! 7) 13

-------------------------

> linesBy :: (t -> Bool) -> [t] -> [[t]]
> linesBy p = anaL' c where
>   c []  = Nothing
>   c xs  = Just $ second (drop 1) $ break p xs

> lb = U.t "lb"
>      (linesBy (==',') "foo,bar,baz")
>      ["foo","bar","baz"]

-------------------------

example: merging lists
----------------------

given 2 sorted, produce 1 sorted list

\small

> mergeLists :: forall a. Ord a => [a] -> [a] -> [a]
> mergeLists = curry $ unfoldr c where
>   c :: Ord a => ([a], [a]) -> Maybe (a, ([a], [a]))
>   c (  [],   [])          = Nothing
>   c (  [], y:ys)          = Just (y, (  [],   ys))
>   c (x:xs,   [])          = Just (x, (  xs,   []))
>   c (x:xs, y:ys) | x <= y = Just (x, (  xs, y:ys))
>                  | x >  y = Just (y, (x:xs,   ys))
>   c (_:_,_:_)             = error "mergeLists"

> ml = U.t "ml"
>      (mergeLists [1,4] [2,3,5::Int])
>      [1,2,3,4,5]

\normalsize

-------------------------

> intToNat :: Int -> Nat
> intToNat = ana coalg where
>     coalg n | n <= 0    = ZeroF
>             | otherwise = SuccF (n - 1)

`coalg` (i.e., "coalgebra")

recursion is not part of the semantics

> itn = U.t "itn"
>       (intToNat 3)
>       (Fix (SuccF (Fix (SuccF (Fix (SuccF (Fix ZeroF)))))))

-------------------------

example: coinductive streams

> -- generates infinite stream
> iterateS :: (a -> a) -> a -> Stream a
> iterateS f = ana' c where
>   c x = S x (f x)

> iterateSL :: (a -> a) -> a -> [a]
> iterateSL f = anaL c where
>   c x = (x, f x)

> sFrom1 :: Stream Integer
> sFrom1 = iterateS (+1) 1

> sFrom1L :: [Integer]
> sFrom1L = iterateSL (+1) 1

> s1s :: [Integer]
> s1s = iterateSL id 1

> takeS :: Int -> Stream a -> [a]
> takeS 0 _                   = []
> takeS n (unCofix -> S x xs) = x : takeS (n-1) xs

> ts = U.tt "ts"
>      [ takeS 6 sFrom1
>      , take  6 sFrom1L
>      ]
>      [1,2,3,4,5,6]

> tss = U.t "tss"
>       (take  6 s1s)
>       [1,1,1,1,1,1]

------------------------------------------------------------------------------

> testAna :: IO Counts
> testAna  =
>     runTestTT $ TestList $ rep ++ fib ++ fib' ++ lb ++ ml ++ itn ++ ts ++ tss
