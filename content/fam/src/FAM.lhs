> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults      #-}
>
> module FAM where
>
> import           Control.Applicative           (liftA, liftA2)
> import           Control.Monad                 (ap, liftM2)
> import           Test.HUnit                    (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util               as U (tt)
>
> default (Int)

Functor
=======

The following are equivalent for functions that take one argument.

> fmapTest = U.tt "fmapTest"
>   [  fmap  (+1)      (Just 1)
>   ,  liftA (+1)      (Just 1)
>   ,        (+1) <$>   Just 1
>   ]
>   (Just 2)

`fmap` will not work for more than one argument.

Applicative
===========

The following are equivalent for functions that take more than one argument.

> liftTest = U.tt "liftTest"
>   [ liftA2 (+)       (Just 1)           (Just 2)
>   ,        (+)  <$>   Just 1  <*>        Just 2
>   ,   Just (+)  <*>   Just 1  <*>        Just 2
>   ]
>   (Just 3)

Monad
=====

> monadTest = U.tt "monadTest"
>   [ liftM2 (+)       (Just 1)           (Just 2)
>   ,  Just  (+)  `ap`  Just 1  `ap`       Just 2
>   ,                   Just 1  >>=  \x -> Just 2 >>= \y -> Just (x + y)
>   ]
>   (Just 3)

Monad and intermediate results
==============================

Since intermediate results are visible when using monads (e.g., `x` and `y` above),
decisions can be made at those points.

> monadTestI = U.tt "monadTestI"
>   [ Just 1  >>=  \x -> if x < 2
>                        then Just 10
>                        else Just  2 >>=  \y -> Just (x + y)
>   ]
>   (Just 10)

Verify.

> main :: IO Counts
> main =
>     runTestTT $ TestList $ fmapTest ++ liftTest ++ monadTest ++ monadTestI
