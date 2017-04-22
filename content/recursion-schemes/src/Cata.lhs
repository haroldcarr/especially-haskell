> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
>
> module Cata where
>
> import           Data.Functor.Foldable (Fix(..))
> import           Fixpoint
> import           ListF
> import           NatF
> import           Prelude               hiding (succ)
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t, tt)

------------------------------------------------------------------------------

Catamorphisms
=============

*cata* meaning *downwards* : generalized fold

- models (internal) *iteration*
- inductive recursion : each recursive step consumes one or more constructors
- ensures terminates (if given finite input)

----

~~~{.haskell}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)
~~~

> cataL :: (a ->        b -> b) -> b -> [a] -> b
> cataL f b (a : as) = f a    (cataL f b as)
> cataL _ b      []  = b

~~~{.haskell}
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
~~~

> cata :: Fixpoint f t => (f a -> a) -> t -> a
> cata alg = alg . fmap (cata alg) . outF

------------------------------------------------------------------------------

> natToInt :: Nat -> Int
> natToInt = cata alg where
>     alg  ZeroF    = 0
>     alg (SuccF n) = n + 1

`alg` (i.e., "algebra") defines reduction semantics

- _semantics are not defined recursively_
- recursion has been factored out: handled by `cata`

> ni = U.t "ni" (natToInt (succ (succ (succ zero)))) 3

----

> -- abbreviation used in equational reasoning
> nia :: Num a => NatF a -> a
> nia  ZeroF    = 0
> nia (SuccF n) = n + 1

~~~{.haskell}
-- derived
instance Functor NatF where
    fmap _ ZeroF     = ZeroF
    fmap f (SuccF z) = SuccF (f z)
~~~

\fontsize{7pt}{7.5}\selectfont

> ni2 = U.tt "ni2"
>   [ natToInt                            (succ       (succ       zero))
>   , natToInt                       (Fix (SuccF (Fix (SuccF (Fix ZeroF)))))
>   ,              cata nia          (Fix (SuccF (Fix (SuccF (Fix ZeroF)))))
>   , (nia . fmap (cata nia) . outF) (Fix (SuccF (Fix (SuccF (Fix ZeroF)))))
>   , (nia . fmap (cata nia))             (SuccF (Fix (SuccF (Fix ZeroF))))
>   ,  nia (SuccF (cata nia                      (Fix (SuccF (Fix ZeroF)))))
>   , 1 +          cata nia                      (Fix (SuccF (Fix ZeroF)))
>   , 1 +   (nia . fmap (cata nia) . outF)       (Fix (SuccF (Fix ZeroF)))
>   , 1 +   (nia . fmap (cata nia))                   (SuccF (Fix ZeroF))
>   , 1 +    nia (SuccF (cata nia                            (Fix ZeroF)))
>   , 1 + 1 +            cata nia                            (Fix ZeroF)
>   , 1 + 1 +           (nia . fmap (cata nia) . outF)       (Fix ZeroF)
>-- , 1 + 1 +           (nia . fmap (cata nia))                   ZeroF
>   , 1 + 1 +            nia                                      ZeroF
>   , 1 + 1 +           0
>   ]
>   2

----

> filterL :: (a -> Bool) -> List a -> List a
> filterL p = cata alg where
>     alg  N                   = nil
>     alg (C x xs) | p x       = cons x xs
>                  | otherwise = xs

~~~{.haskell}
-- derived
instance Functor (ListF a) where
  fmap _ N        = N
  fmap f (C x xs) = C x (f xs)
~~~

> fi = U.t "fi"
>      (filterL even (cons 1 (cons (2::Int) nil)))
>                            (cons  2       nil)

------------------------------------------------------------------------------

> testCata :: IO Counts
> testCata  =
>     runTestTT $ TestList $ ni ++ ni2 ++ fi
