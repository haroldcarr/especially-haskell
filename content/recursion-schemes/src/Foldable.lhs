> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
>
> module Foldable where
>
> import           Data.Foldable         (fold)
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t)
> import           TreeF
>
> {-# ANN module "HLint: ignore Use sum"   #-}

Foldable : generalize `fold` to work on other types
========================================

Process elements of a structure one-at-a-time, discarding the structure.

~~~{.haskell}
class Foldable t where
   -- | Right-associative fold of structure.
   foldr :: (a -> b -> b) -> b -> t a -> b
-- compare with list-based `foldr` sig:
-- foldr :: (a -> b -> b) -> b -> [a] -> b
~~~

----

example data

> f1 :: T' Int
> f1 = B' (B' (B' (L'  1)   2 E')
>         3
>         (L'  4))
>      5
>      E'

> f2 :: T' [Int]
> f2 = B' (B' (B' (L' [1]) [2] E')
>         [3]
>         (L' [4]))
>      [5]
>      E'

Foldable typeclass operations
=============================

~~~{.haskell}
-- | Right-associative fold of structure.
foldr :: (a -> b -> b) -> b -> t a -> b
~~~

> f1foldr  = U.t "f1foldr"
>            (foldr (+) 0 f1)
>            15

> f1foldr2 = U.t "f1foldr2"
>            (foldr ((++) . show . head) "" f2)
>            "12345"

----

Other `Foldable` operations

~~~{.haskell}
-- | Combine elements of structure using a monoid.
fold :: Monoid m => t m -> m
~~~

> f2fold    = U.t "f2fold"
>             (fold f2)
>             [1,2,3,4,5]

~~~{.haskell}
-- | Map each element of structure to a monoid
-- and combine results.
foldMap :: Monoid m => (a -> m) -> t a -> m
~~~

> f1foldMap = U.t "f1foldMap"
>             (foldMap show f1)
>             "12345"

----

note on `Data.Monoid`

Types with associative binary operation and identity.\
Instances must should satisfy laws:

- `mappend mempty x        = x`
- `mappend x mempty        = x`
- `mappend x (mappend y z) = mappend (mappend x y) z`
- `mconcat                 = foldr mappend mempty`

~~~{.haskell}
class Monoid a where
  -- | Identity of 'mappend'
  mempty  :: a
  -- | Associative operation
  mappend :: a -> a -> a
  -- | Fold a list using the monoid.
  mconcat :: [a] -> a
~~~

----

other `Foldable` operations

~~~{.haskell}
-- | foldr with no base case. For non-empty structures.
foldr1 :: (a -> a -> a) -> t a -> a

-- | List elements of structure, left to right.
toList :: t a -> [a]

-- | Is structure empty?.
null :: t a -> Bool
~~~

----

~~~{.haskell}
-- | size/length of structure (e.g., num elements)
length :: t a -> Int

-- | Does element occur in structure?
elem :: Eq a => a -> t a -> Bool

-- | Largest element of non-empty structure.
maximum :: forall a . Ord a => t a -> a

-- | Sum the numbers of structure.
sum :: Num a => t a -> a

-- | Multiply the numbers of a structure.
product :: Num a => t a -> a
~~~

----

non-TC functions on `Foldable`

- concat, concatMap,
- and, or, any, all,
- maximumBy, minimumBy,
- notElem, find

------------------------------------------------------------------------------

> testFoldable :: IO Counts
> testFoldable  =
>     runTestTT $ TestList $ f1foldr ++ f1foldr2 ++ f2fold ++ f1foldMap
