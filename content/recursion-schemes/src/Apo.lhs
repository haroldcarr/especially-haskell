> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults      #-}
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

\textbf{apomorphism}
--------------------

*apo* meaning *apart* : categorical dual of paramorphism and extension anamorphism (coinduction) [6].

- models *primitive corecursion* over a coinductive type
- enables short-circuiting traversal and immediately deliver a result

- para gives access cursor at every point of the recursion
- apo enables terminating recursion at any time

TODO http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.35.7317&rep=rep1&type=pdf
 https://jtobin.io/sorting-slower-with-style
 https://books.google.com/books?id=xycOgASxEWQC&pg=PA155&lpg=PA155&dq=apomorphism&source=bl&ots=WYGhIdOAgy&sig=DpbWd5S4dgUb89PllF4kMOWEu14&hl=en&sa=X&ved=0ahUKEwjL3_jYsKjMAhUks4MKHc2nDBo4FBDoAQglMAM#v=onepage&q=apomorphism&f=false

-- primitive corecursion (dual of para)

> apo :: Fixpoint f t => (a -> f (Either a t)) -> a -> t
> apo coa = inF . fmap (apo coa ||| id) . coa

- can also be expressed in terms of an anamorphism

~~~{.haskell}
apo :: Fixpoint f t => (a -> f (Either a t)) -> a -> t
apo coa = ana (coa ||| fmap Right . outF) . Left
~~~

> apoL :: ([b] -> Maybe (a, Either [b] [a])) -> [b] -> [a]
> apoL f b = case f b of
>   Nothing -> []
>   Just (x, Left c)  -> x : apoL f c
>   Just (x, Right e) -> x : e

> -- TODO : what is this?
> -- on finite inputs, same as `anaL` but with results of `h` applied to final element appended
> apoL' :: (b -> Maybe (a, b)) -> (b -> [a]) -> b -> [a]
> apoL' f h b = case f b of
>   Just (a, b') -> a : apoL' f h b'
>   Nothing      -> h b

> ap = apoL' f h
>  where
>   f    []  = Nothing
>   f (x:xs) = if x < 5 then Just (x*10,xs) else Nothing
>   h = id

> ae = U.t "ae"
>      (ap [1,3,5,7,9])
>      [10,30,5,7,9]

------------------------------------------------------------------------------

example: uses apomorphism to generate

- new insertion step when `x>y`
- short-circuits to final result when `x<=y`

> insertElemL :: Ord a => a -> [a] -> [a]
> insertElemL a as = apoL c (a:as) where
>   c      []           = Nothing
>   c     [x]           = Just (x, Left     [])
>   c (x:y:xs) | x <= y = Just (x, Right (y:xs)) -- DONE
>              | x >  y = Just (y, Left  (x:xs))
>   c _                 = error "insertElem"

> iel = U.t "iel"
>      (insertElemL 3 [1,2,5])
>      [1,2,3,5]

> insertElem :: forall a. Ord a => ListF a [a] -> [a]
> insertElem = apo c where
>   c :: Ord a => ListF a [a] -> ListF a (Either (ListF a [a]) [a])
>   c N                     = N
>   c (C x    [])           = C x (Left N)
>   c (C x (y:xs)) | x <= y = C x (Right (y:xs))
>                  | x >  y = C y (Left (C x xs))
>   c _                     = error "insertElem"

> ie = U.t "ie"
>      (insertElem (C 3 [1,2,5]))
>      [1,2,3,5]

----

example: insert every element of supplied list into a new list, using `cata`

> insertionSort :: Ord a => [a] -> [a]
> insertionSort = cata insertElem

> iss = U.t "iss"
>       (insertionSort [5,9,3,1,2])
>       [1,2,3,5,9]

------------------------------------------------------------------------------

> testApo :: IO Counts
> testApo  =
>     runTestTT $ TestList $ ie ++ ae ++ iel ++ iss
