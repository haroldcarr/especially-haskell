> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
>
> {-# LANGUAGE RankNTypes #-}
>
> module Apo where
>
> import           Cata                  (cata)
> import           Control.Arrow         ((|||))
> import           Fixpoint
> import           ListF
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t)

------------------------------------------------------------------------------

Apomorphisms
===========

*apo* meaning *apart* : categorical dual of paramorphism and extension anamorphism (coinduction) [6].

- models *primitive corecursion* over a coinductive type
- enables short-circuiting traversal and immediately deliver a result

TODO http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.35.7317&rep=rep1&type=pdf
https://books.google.com/books?id=xycOgASxEWQC&pg=PA155&lpg=PA155&dq=apomorphism&source=bl&ots=WYGhIdOAgy&sig=DpbWd5S4dgUb89PllF4kMOWEu14&hl=en&sa=X&ved=0ahUKEwjL3_jYsKjMAhUks4MKHc2nDBo4FBDoAQglMAM#v=onepage&q=apomorphism&f=false

-- primitive corecursion (dual of para)
-- on finite inputs, same as `anaL` but with results of `h` applied to final element appended

> apoL  :: (b -> Maybe (a, b)) -> (b -> [a]) -> b -> [a]
> apoL f h b = case f b of
>   Just (a, b') -> a : apoL f h b'
>   Nothing      -> h b

> apo :: Fixpoint f t => (a -> f (Either a t)) -> a -> t
> apo coa = inF . fmap (apo coa ||| id) . coa

- can also be expressed in terms of an anamorphism

~~~{.haskell}
apo :: Fixpoint f t => (a -> f (Either a t)) -> a -> t
apo coa = ana (coa ||| fmap Right . outF) . Left
~~~

------------------------------------------------------------------------------

example: uses apomorphism to generate

- new insertion step when `x>y`
- short-circuits to final result when `x<=y`

> insertElem :: forall a. Ord a => ListF a [a] -> [a]
> insertElem = apo c where
>   c :: Ord a => ListF a [a] -> ListF a (Either (ListF a [a]) [a])
>   c N                     = N
>   c (C x [])              = C x (Left N)
>   c (C x (y:xs)) | x <= y = C x (Right (y:xs))
>                  | x > y  = C y (Left (C x xs))
>   c _                     = error "insertElem"

> ie = U.t "ie"
>      (insertElem (C 3 [1::Int,2,5]))
>      [1,2,3,5]

----

example: insert every element of supplied list into a new list, using `cata`

> insertionSort :: Ord a => [a] -> [a]
> insertionSort = cata insertElem

> iss = U.t "iss"
>       (insertionSort [5,9,3,1,2::Int])
>       [1,2,3,5,9]

------------------------------------------------------------------------------

> testApo :: IO Counts
> testApo  =
>     runTestTT $ TestList $ ie ++ iss
