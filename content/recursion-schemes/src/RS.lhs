Refactoring Recursion
=====================

\begin{center}

\LARGE{Harold Carr}

\end{center}

- Tim Williams's recursion schemes presentation
    - http://www.timphilipwilliams.com/slides.html
    - https://www.youtube.com/watch?v=Zw9KeP3OzpU


Introduction
============

- Recursion is a pattern
- There are different patterns of recursion
- "factoring" recursion : benefits
    - communicate/reason about programs
    - code/idea reuse
    - use a catalogue of theorems to optimise or prove properties
    - identify/exploit parallelism

Overview
========

- explicit recursive functions
- factor recursion out of functions with `fold`
- explicit recursive data
- factor recursion out of data with `Fix`
- use "library" functions to recursivly operate on "fixed" data
    - generic "folds" (aka "catamorphism")
    - generic "unfolds" (aka "anamorphism")
    - generic unfolds followed by folds (aka "hylomorphism")
- conclusion
- other recursion schemes (time permitting)
- foundations and advanced usage (in the hall)

----

\iffalse

> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
>
> {-# LANGUAGE DeriveFoldable         #-}
> {-# LANGUAGE DeriveFunctor          #-}
> {-# LANGUAGE DeriveTraversable      #-}
> {-# LANGUAGE FlexibleContexts       #-}
> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE RankNTypes             #-}
> {-# LANGUAGE ScopedTypeVariables    #-}
> {-# LANGUAGE TupleSections          #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE UndecidableInstances   #-}
> {-# LANGUAGE ViewPatterns           #-}
>
> module RS where
>
> import           Control.Applicative   (empty, many, (<|>))
> import           Control.Arrow         (second, (&&&), (***), (|||))
> import           Control.Monad.Reader  (ReaderT, ask, fix, lift, runReaderT, (<=<))
> import           Control.Monad.ST      (ST, runST)
> import           Data.Foldable         (fold)
> import           Data.Functor.Classes  (Eq1(..), Show1(..), showsUnaryWith)
> import           Data.Functor.Foldable (Fix(..))
> import           Data.Functor.Identity as DFI (Identity (..))
> import           Data.List.Ordered     as O (merge)
> import           Data.Map              as M (Map, fromList, lookup)
> import           Data.Maybe            (fromMaybe, isJust)
> import           Data.Monoid           (Sum (..), getSum, (<>))
> import           Data.Set              as S (Set, fromList, singleton)
> import           Numeric               (readFloat, readSigned)
> import           Prelude               as P hiding (replicate, succ)
>
> -- Third-party Hackage packages
>
> import           Data.Bool.Extras              (bool)
> import           Data.Hashable                 (Hashable, hashWithSalt)
> import           Data.HashTable.Class          (HashTable)
> import qualified Data.HashTable.Class          as H (insert, lookup, new)
> import qualified Data.HashTable.ST.Cuckoo      as C (HashTable)
> import           Test.HUnit                    (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util               as U (t, tt)
> import           Text.Parsec.Prim              (ParsecT)
> import           Text.ParserCombinators.Parsec hiding (many, space, (<|>))
> import           Text.PrettyPrint.Leijen       (Doc, Pretty, pretty, space, text, (<+>))
> import qualified Text.PrettyPrint.Leijen       as PP (brackets, (<>))
>
> {-# ANN module "HLint: ignore Use foldr" #-}
> {-# ANN module "HLint: ignore Use sum"   #-}
> {-# ANN module "HLint: ignore Use and"   #-}
> {-# ANN factEqR "HLint: ignore"          #-}
> {-# ANN foldrX "HLint: ignore"           #-}
> {-# ANN fxl "HLint: ignore"              #-}
>

\fi

> -- TODO : without this, pandoc/pdf conversion complains

Explicit Recursion
==================

note the pattern

> sumE    []  = 0
> sumE (x:xs) = x +  sumE xs

> andE    []  = True
> andE (x:xs) = x && andE xs

same recursive structure, except

- `0` or `True`
    - for base case (i.e., empty list)
- `+` or `&&`
    - for operator in inductive case

factor recursion out of functions with `fold`
============================================

\iffalse

> sumF :: (Foldable t, Num b) => t b    -> b

\fi

> sumF  = foldr (+)  0

\iffalse

> andF :: Foldable t          => t Bool -> Bool

\fi

> andF  = foldr (&&) True

~~~{.haskell}
     sumF                  andF
      +                     &&
     / \                   /  \
    1   +               True   &&
       / \                    /  \
      2   +               False   &&
         / \                     /  \
        3   0                 True  True
~~~

----

another example: `length`

> lengthE []     = 0
> lengthE (_:xs) = 1 + lengthE xs

as a fold

\iffalse

> lengthF :: (Foldable t, Num b) => t a -> b

\fi

> lengthF        = foldr  (\_ n -> 1 + n)  0

~~~{.haskell}
lengthFL       = foldl' (const . P.succ) 0

 sumF                  andF                  lengthF
  +                     &&                      1+
 / \                   /  \                    /  \
1   +               True   &&                 _    1+
   / \                    /  \                    /  \
  2   +               False   &&                 _    1+
     / \                     /  \                    /  \
    3   0                 True  True                _    0
~~~

----

to understand how this works, look at def of `foldr` :

~~~{.haskell}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)
~~~

~~~{.haskell}
 sumF                  andF                  lengthF
  +                     &&                      1+
 / \                   /  \                    /  \
1   +               True   &&                 _    1+
   / \                    /  \                    /  \
  2   +               False   &&                 _    1+
     / \                     /  \                    /  \
    3   0                 True  True                _    0
~~~

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

> data Tree a = E
>             | L a
>             | B (Tree a) a (Tree a)
>             deriving (Eq, Foldable, Functor, Show)

derive using `{-# LANGUAGE DeriveFoldable #-}`

----

example data

> f1 :: Tree Int
> f1 = B (B (B (L  1)   2 E)
>         3
>         (L  4))
>      5
>      E

> f2 :: Tree [Int]
> f2 = B (B (B (L [1]) [2] E)
>         [3]
>         (L [4]))
>      [5]
>      E

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

factor recursion out of data types with `Fix`
=============================================

- benefits:
    - reason about recursion and base structures separately
- applicable to types that have recursive structure
    - e.g., lists, trees

~~~{.haskell}
data Natural = Zero | Succ Natural
~~~

----

~~~{.haskell}
data Natural = Zero | Succ Natural
~~~

factor out recursion by defining base structure

- with parameterized type at recursive points

> -- "pattern functor" for `Natural` (must be a functor)
> data NatF r = ZeroF | SuccF r
>             deriving (Eq, Functor, Show)

add recursion to pattern functors via recursive `Fix` wrapper

> type Nat    = Fix NatF

----

`Fix` definition

~~~{.haskell}
newtype Fix f = Fix { unFix :: f (Fix f) }

fix          :: f -> Fix f
fix           = Fix
~~~

> -- think of `unFix` as
> unFix        :: Fix f -> f (Fix f)
> unFix (Fix f) = f

- `fix` adds one level of recursion
- `unfix` removes one level of recursion

`Fix` *is "generic" recursive structure*

- write recursive type without using recursion
- use `Fix` to add recursion

Working with fixed data-types
=============================

a type class (using `FunctionalDependencies` [FD]) to
(transparently) apply isomorphism between (un)fixed representations

> class Functor f => Fixpoint f t | t -> f where
>   inF  :: f t -> t
>   outF :: t -> f t

> instance Functor f => Fixpoint f (Fix f) where
>   inF  = Fix
>   outF = unFix

> ifi = U.t "ifi"
>     (inF       (SuccF (Fix ZeroF)))
>           (Fix (SuccF (Fix ZeroF)))

> ofi = U.t "ofi"
>     (outF (Fix (SuccF (Fix ZeroF))))
>                (SuccF (Fix ZeroF))

----

smart constructors

> zero  :: Nat
> zero  = Fix ZeroF

> succ  :: Nat -> Nat
> succ  = Fix . SuccF

> sc = U.t "sc"
>      (succ       (succ       (succ        zero     )))
>      (Fix (SuccF (Fix (SuccF (Fix (SuccF (Fix ZeroF)))))))

----

example : lists

> data ListF a r = C a r | N deriving (Eq, Functor, Show)

> type List a    = Fix (ListF a)

> nil           :: List a
> nil            = Fix N
> cons          :: a -> List a -> List a
> cons x xs      = Fix (C x xs)

> instance Fixpoint (ListF a) [a] where
>   inF N        = []
>   inF (C x xs) = x : xs
>   outF []      = N
>   outF (x:xs)  = C x xs

----

example : trees

~~~{.haskell}
data TreeF a r = EmptyF
               | LeafF a
               | NodeF r r
              deriving (Show, Functor)

type Tree a    = Fix (TreeF a)
~~~

recusion as library functions
=============================

\begin{tabular}{ l l p{6cm} }
{\tt cata}  &    catamorphism  & folds \\
{\tt ana}   &    anamorphisms  & unfolds \\
{\tt hylo}  &    hylomorphism  & {\tt ana} then {\tt cata} (corecursive production followed by recursive consumption) \\
.           &    .             & .   \\
.           &    .             & .   \\
.           &    .             & .   \\
\end{tabular}



Catamorphisms
=============

*cata* meaning *downwards* : generalized fold

- models (internal) *iteration*
- goal: write `foldr` once for all data-types

----

~~~{.haskell}
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
~~~

> cata :: Fixpoint f t => (f a -> a) -> t -> a
> cata alg = alg . fmap (cata alg) . outF

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


Anamorphisms
============

*ana* meaning *upwards* : generalized unfold

- corecursive dual of catamorphisms
- produces streams and other regular structures from a seed
- `ana` for lists is `unfoldr` (`ViewPatterns` help see the duality)

~~~{.haskell}
foldrP  :: (Maybe (a, b) -> b) -> [a] -> b
foldrP f []     = f Nothing
foldrP f (x:xs) = f (Just (x, foldrP f xs))
~~~

> unfoldr :: (b -> Maybe (a, b)) ->  b -> [a]
> unfoldr f (f -> Nothing)                   = []
> unfoldr f (f -> Just (x, unfoldr f -> xs)) = x : xs

\iffalse

> unfoldr _ _ = matchAll "unfoldr"

\fi

----

list examples
------------------------------------------------------

> replicate :: Int -> a -> [a]
> replicate n0 x = unfoldr c n0 where
>     c 0 = Nothing
>     c n = Just (x, n-1)

> rep = U.t "rep" (replicate 4 '*') "****"

> linesBy :: (t -> Bool) -> [t] -> [[t]]
> linesBy p = unfoldr c where
>     c []  = Nothing
>     c xs  = Just $ second (drop 1) $ break p xs

> lb = U.t "lb"
>      (linesBy (==',') "foo,bar,baz")
>      ["foo","bar","baz"]

----

example: merging lists
----------------------

given two sorted lists, `mergeLists` merges them into one sorted list

> mergeLists :: forall a. Ord a => [a] -> [a] -> [a]
> mergeLists = curry $ unfoldr c where
>   c :: ([a], [a]) -> Maybe (a, ([a], [a]))
>   c ([], [])              = Nothing
>   c ([], y:ys)            = Just (y, ([], ys))
>   c (x:xs, [])            = Just (x, (xs, []))
>   c (x:xs, y:ys) | x <= y = Just (x, (xs, y:ys))
>                  | x > y  = Just (y, (x:xs, ys))
>   c (_:_,_:_)             = matchAll "mergeLists"

> ml = U.t "ml"
>      (mergeLists [1,4] [2,3,5::Int])
>      [1,2,3,4,5]

----

Corecursion
-----------

anamorphism is *corecursive*

- dual of catamorphisms / recursion

corecursion produces (potentially infinite) *codata*

recursion consumes (necessarily finite) *data*

----

No checked distinction between data and codata in Haskell

- so use of `Fix` again

> -- | anamorphism
> ana :: Functor f => (a -> f a) -> a -> Fix f
> ana coalg = Fix . fmap (ana coalg) . coalg
>

compare to

~~~{.haskell}
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg  = alg . fmap (cata alg)  . unFix
~~~

----

> intToNat :: Int -> Nat
> intToNat = ana coalg where
>     coalg n | n <= 0    = ZeroF
>             | otherwise = SuccF (n - 1)

`coalg` (i.e., "coalgebra")

recursion is not part of the semantics

> itn = U.t "itn"
>       (intToNat 3)
>       (Fix (SuccF (Fix (SuccF (Fix (SuccF (Fix ZeroF)))))))

----

to distinquish data/codata (useful when working with streams)

> newtype Cofix f = Cofix { unCofix :: f (Cofix f) }

compare to

~~~{.haskell}
newtype Fix   f = Fix   { unFix   :: f (Fix   f) }
~~~

use in `ana` definition

> -- | an alternative anamorphism typed for codata
> ana' :: Functor f => (a -> f a) -> a -> Cofix f
> ana' coalg = Cofix . fmap (ana' coalg) . coalg


----

example: coinductive streams
----------------------------

> -- like `List` but no base case (i.e., Nil)
> data StreamF a r = S a r deriving (Functor, Show)
> type Stream a = Cofix (StreamF a)

~~~{.haskell}
-- derived
instance Functor (StreamF a) where
  fmap f (S x xs) = S x (f xs)
~~~

constructor/deconstructors:

> consS :: a -> Cofix (StreamF a) -> Cofix (StreamF a)
> consS x xs = Cofix (S x xs)

> headS :: Cofix (StreamF a) -> a
> headS (unCofix -> (S x _ )) = x

> tailS :: Cofix (StreamF a) -> Cofix (StreamF a)
> tailS (unCofix -> (S _ xs)) = xs

----

> -- generates infinite stream
> iterateS :: (a -> a) -> a -> Stream a
> iterateS f = ana' c where
>   c x = S x (f x)

> s1 :: Stream Integer
> s1 = iterateS (+1) 1

> takeS :: Int -> Stream a -> [a]
> takeS 0 _                   = []
> takeS n (unCofix -> S x xs) = x : takeS (n-1) xs

> ts = U.t "ts"
>      (takeS 6 s1)
>      [1,2,3,4,5,6]


Hylomorphism
============

composition of catamorphism and anamorphism

- corecursive codata production followed by recursive data consumption
- can express general computation
- models *general recursion*
- enables replacing any recursive control structure with a data structure
- a representation enables exploiting parallelism

> hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
> hylo g h = cata g . ana h

NB. termination not guaranteed

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
>       (mergeSort [7,6,3,1,5,4,2::Int])
>       [1,2,3,4,5,6,7]

\begin{picture}(0,0)(0,0)
\put(165,-50){\resizebox{2in}{!}{%
\begin{tikzpicture}[]
\Tree [.Bin [.Bin [.Leaf 7 ] [.Bin [.Leaf 6 ] [.Leaf 3 ] ] ] [.Bin [.Bin [.Leaf 1 ] [.Leaf 5 ] ] [.Bin [.Leaf 4 ] [.Leaf 2 ] ] ] ]
\end{tikzpicture}}}
\end{picture}


Conclusion
==========

- catamorphisms, anamorphisms hylomorphisms
    - folds, unfolds, and refolds
    - fundamental
    - they can express all recursive computation
- other recursion schemes : based on the above
    - offer more structure
- enable reliable, efficient, parallel programs

Recursion   Corecursion   General
----------  ------------  ---------
cata        ana           hylo
para        apo
histo       futu
zygo


OTHER RECURSION SCHEMES
=======================

\begin{tabular}{ l l p{6cm} }
`para`  &    paramorphism  & folds with access to input arg corresponding to most recent state of computation \\
`apo`   &    apomorphism   & TODO unfold that can exit early \\
`zygo`  &    zygomorphism  & TODO generalized paramorphism : asymmetric mutual recursion \\
`histo` &    histomorphism & TODO course-of-value recursion : can use previously computed values \\
`futu`  &    futumorphism  & TODO course-of-value coiteration : produce one or more levels \\
\end{tabular}

Paramorphisms
=============

*para* meaning *beside* : extension of catamorphism

- models *primitive recursion* over an inductive type
- enables access to the original input structures
- operates on algebra that provides access to input arg corresponding to running state of the recursion

~~~{.haskell}
para :: Fixpoint f t => (f (a, t) -> a ) -> t -> a
para alg = fst . cata (alg &&& Fix . fmap snd)
~~~~

> -- more efficient
> para :: Fixpoint f t => (f (a, t) -> a) -> t -> a
> para alg = alg . fmap (para alg &&& id) . outF

-- (&&&) :: (b -> c) -> (b -> d) -> (b -> (c, d))

----

- factorial : classic example of primitive recursion
- usual `fact n = foldr (*) [1..n]` is unfold followed by fold

> fact :: Integer -> Integer
> fact = para alg where
>   alg ZeroF          = 1
>   alg (SuccF (f, n)) = f * (n + 1)

> fct = U.t "fct" (fact 10) 3628800

> instance Fixpoint NatF Integer where
>   inF ZeroF          = 0
>   inF (SuccF n)      = n + 1
>   outF n | n > 0     = SuccF (n - 1)
>          | otherwise = ZeroF

> infz = U.t "infz" (inF ZeroF     :: Integer) 0
> infs = U.t "infs" (inF (SuccF 0) :: Integer) 1
> otfz = U.t "otfz" (outF 0 :: NatF Integer) ZeroF
> otfs = U.t "otfs" (outF 1 :: NatF Integer) (SuccF 0)

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

Apomorphisms
===========

*apo* meaning *apart* : categorical dual of paramorphism and extension anamorphism (coinduction) [6].

- models *primitive corecursion* over a coinductive type
- enables short-circuiting traversal and immediately deliver a result

> apo :: Fixpoint f t => (a -> f (Either a t)) -> a -> t
> apo coa = inF . fmap (apo coa ||| id) . coa

- can also be expressed in terms of an anamorphism

~~~{.haskell}
apo :: Fixpoint f t => (a -> f (Either a t)) -> a -> t
apo coa = ana (coa ||| fmap Right . outF) . Left
~~~

----

example: uses apomorphism to generate

- new insertion step when `x>y`
- short-circuits to final result when `x<=y`

> insertElem :: forall a. Ord a => ListF a [a] -> [a]
> insertElem = apo c where
>   c :: ListF a [a] -> ListF a (Either (ListF a [a]) [a])
>   c N                     = N
>   c (C x [])              = C x (Left N)
>   c (C x (y:xs)) | x <= y = C x (Right (y:xs))
>                  | x > y  = C y (Left (C x xs))
>   c _                     = matchAll "insertElem"

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

Zygomorphism
============

- asymmetric mutual iteration
    - both a data consumer and an auxiliary function are defined
- a generalisation of paramorphisms

> algZygo :: Functor f =>
>     (f     b  -> b) ->
>     (f (a, b) -> a) ->
>      f (a, b)       ->
>        (a, b)
> algZygo f g = g &&& f . fmap snd

> zygo :: Functor f =>
>         (f b -> b) -> (f (a, b) -> a) -> Fix f -> a
> zygo f g = fst . cata (algZygo f g)

----

example: using evaluation to find discontinuities
-------------------------------------------------

count number of live conditionals causing discontinuities due to an arbitrary supplied environment,
in a single traversal.

`discontAlg` : one of its embedded arguments, result of evaluating current term using env

> discontAlg :: ExprF (Sum Int, Maybe Int) -> Sum Int
> discontAlg (IfNeg (t, tv) (x, xv) (y, yv))
>   | isJust xv, isJust yv, xv == yv =  t <> x <> y
>   | otherwise = maybe (Sum 1 <> t <> x <> y)
>                       (bool (t <> y) (t <> x) . (<0)) tv
> discontAlg e = fold . fmap fst $ e

note: check for redundant live conditionals for which both branches evaluate to same value

----

> -- | number of live conditionals
> disconts :: Env -> Expr -> Int
> disconts env = getSum . zygo (evalAlg env) discontAlg

- expression `e2` is a function of variables `a` and `b`

> e2 :: Fix ExprF
> e2 = Fix (IfNeg (Fix (Var "b")) e1 (Fix (Const 4)))

> fve2 = U.t "fve2"
>        (freeVars e2)
>        (S.fromList ["a","b"])

----

- by supplying `disconts` with a value for `b`,
  can look for discontinuities with respect to a new function over just `a`

> ofe2 = U.t "ofe2"
>        (optimizeFast e2)
>        (Fix
>         (IfNeg (Fix (Var "b"))
>          (Fix (Mul (Fix (IfNeg (Fix (Var "a"))
>                          (Fix (Var "b"))
>                          (Fix (Add (Fix (Var "b")) (Fix (Const 2))))))
>                (Fix (Const 3))))
>          (Fix (Const 4))))

> di = U.t "di"
>      (disconts (M.fromList [("b",-1)]) e2)
>      1


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

----

example: computing Fibonacci numbers
------------------------------------

> fib :: Integer -> Integer
> fib = histo f where
>   f :: NatF (Ann NatF Integer) -> Integer
>   f ZeroF                                         = 0
>   f (SuccF (unAnn -> (ZeroF,_)))                  = 1
>   f (SuccF (unAnn -> (SuccF (unAnn -> (_,n)),m))) = m + n
>   f _    = matchAll "fib"

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
>   alg _                      = matchAll "evens"

> ev = U.t "ev"
>      (evens [1..7::Int])
>      [2,4,6]


Futumorphism
============

- REF: introduced by Uustalu & Venu in 1999 [7]
- the corecursive dual of the histomorphism
- models *course-of-value* coiteration
- enables producing one or more levels

> futu :: Functor f => (a -> f (Ctx f a)) -> a -> Cofix f
> futu coa = ana' ((coa ||| id) . unCtx) . hole

> -- | deconstruct values of type Ctx f a
> unCtx :: Ctx f a -> Either a (f (Ctx f a))
> unCtx c = case unFix c of
>   Hole x -> Left x
>   Term t -> Right t

> term :: f (Fix (CtxF f a)) -> Fix (CtxF f a)
> term = Fix . Term
>
> hole :: a -> Fix (CtxF f a)
> hole = Fix . Hole

----

example: stream processing
--------------------------

pairwise exchanges elements of stream

> exch :: Stream a -> Stream a
> exch = futu coa where
>   coa xs = S (headS $ tailS xs)
>              (term $ S (headS xs)
>                        (hole $ tailS $ tailS xs))

> exs = U.t "exs"
>       (takeS 10 $ exch s1)
>       [2,1,4,3,6,5,8,7,10,9]


ADVANCED
========

----

Catamorphism
------------

\vspace{0.2in}
\centerline{\resizebox{3in}{!}{%
\begin{tikzpicture}[node distance=2.75cm, auto, font=\small\sffamily]
  \node (ffixf) {\bf\it f (Fix f)};
  \node (fixf) [below of=ffixf] {\bf\it Fix f};
  \node (fa) [right of=ffixf] {\bf\it f a};
  \node (a) [right of=fixf] {\bf\it a};
  \draw[->] (ffixf) to node {\tiny fmap (cata alg)} (fa);
  \draw[->] (ffixf) to node [swap] {\tiny Fix} (fixf);
  \draw[->] (fixf) to node [swap] {\tiny cata alg} (a);
  \draw[->] (fa) to node {\tiny alg} (a);
\end{tikzpicture}
}}

----

concrete example

~~~{.haskell}
foldr :: (a -> b -> b) -> z -> [a] -> b
foldr f z    []     = z
foldr f z    (x:xs) = f          x (foldr f z  xs)
~~~

written in form closer to theory

> foldrP :: (Maybe (a, b) -> b) -> [a] -> b
> foldrP alg []     = alg Nothing
> foldrP alg (x:xs) = alg (Just (x, foldrP alg xs))

----

~~~{.haskell}
foldrP :: (Maybe (a, b) -> b) -> [a] -> b
foldrP alg []     = alg Nothing
foldrP alg (x:xs) = alg (Just (x, foldrP alg xs))
~~~

> mp Nothing        = 0
> mp (Just (a, bs)) = a + bs

> fpl = U.tt "fpl"
>       [                      foldrP mp [1,2::Int]
>       ,         mp (Just (1, foldrP mp   [2]))
>       , 1 +                  foldrP mp   [2]
>       , 1 +     mp (Just (2, foldrP mp   []))
>       , 1 + 2 +              foldrP mp   []
>       , 1 + 2 + mp           Nothing
>       , 1 + 2 + 0
>       ]
>       3

----

> lengthX :: [a] -> Int
> lengthX = foldrP alg where
>   alg :: Maybe (a, Int) -> Int
>   alg Nothing        = 0
>   alg (Just (_, xs)) = xs + 1

> lx = U.t "lx" (lengthX "foobar") 6

----

written in form even closer to theory

> foldrX :: (Maybe (a, b) -> b) -> [a] -> b
> foldrX alg = alg . fmap (id *** foldrX alg) . unList
>   where
>     unList []     = Nothing
>     unList (x:xs) = Just (x, xs)

- Uses function product \footnote{defined more generally in Control.Arrow}

~~~{.haskell}
(***) :: (b -> c) -> (d -> e) -> (b, d) -> (c, e)
(f *** g) (x, y) = (f x, g y)
~~~

----

~~~{.haskell}
foldrX :: (Maybe (a, b) -> b) -> [a] -> b
foldrX alg = alg . fmap (id *** foldrX alg) . unList
  where
    unList []     = Nothing
    unList (x:xs) = Just (x, xs)
~~~

> fxl = U.tt "fxl"
>       [                            foldrX mp         [1, 2::Int]
>       ,         (mp . fmap (id *** foldrX mp)) (Just (1,[2]))
>       ,          mp   (Just (1,    foldrX mp            [2]))
>       , 1 +                        foldrX mp            [2]
>       , 1 +     (mp . fmap (id *** foldrX mp)) (Just (2,[]))
>       , 1 +      mp   (Just (2,    foldrX mp            []))
>       , 1 + 2 +                    foldrX mp            []
>       , 1 + 2 + (mp . fmap (id *** foldrX mp)) Nothing
>       , 1 + 2 +  mp   Nothing
>       , 1 + 2 + 0
>       ]
>       3

----

`foldrX` can be visualized:\footnote{The nodes represent types (objects) and the edges functions (morphisms).}

\vspace{0.2in}
\centerline{\resizebox{4in}{!}{%
\begin{tikzpicture}[auto, font=\small\sffamily]
  \node (ffixf) at (0,2.75) {\bf\it Maybe (a, [a])};
  \node (fixf) at (0,0) {\bf\it [a]};
  \node (fa) at (4.5,2.75) {\bf\it Maybe (a, b)};
  \node (a) at (4.5,0) {\bf\it b};
  \draw[->] (ffixf) to node {\tiny fmap (id *** foldrX alg)} (fa);
  \draw[->] (fixf) to node {\tiny unList} (ffixf);
  \draw[->] (fixf) to node [swap] {\tiny foldrX alg} (a);
  \draw[->] (fa) to node {\tiny alg} (a);
\end{tikzpicture}
}}

----

Anamorphism
-----------

\vspace{0.2in}
\centerline{\resizebox{3in}{!}{%
\begin{tikzpicture}[node distance=2.75cm, auto, font=\small\sffamily]
  \node (ffixf) {\bf\it f (Cofix f)};
  \node (fixf) [below of=ffixf] {\bf\it Cofix f};
  \node (fa) [right of=ffixf] {\bf\it f a};
  \node (a) [right of=fixf] {\bf\it a};
  \draw[->] (fa) to node [swap] {\tiny fmap (ana coalg)} (ffixf);
  \draw[->] (fixf) to node {\tiny unFix} (ffixf);
  \draw[->] (a) to node [swap] {\tiny ana coalg} (fixf);
  \draw[->] (a) to node [swap] {\tiny coalg} (fa);
\end{tikzpicture}
}}

----

catamorphism-fusion law [3]
----------------------------

transform composition of a function with a catamorphism into single catamorphism

- eliminates intermediate data structures

$$h \: . \: f = g\: . \: fmap \: h \implies h \: . \: cata \: f = cata \: g$$

where

~~~{.haskell}
f :: f a -> a
g :: f b -> b
h :: a -> b
~~~

\begin{picture}(0,0)(0,0)
\put(225,0){\includegraphics[height=1in]{images/fusion.png}}
\end{picture}

----

example: expressions
-------------------------------------

> data ExprF r = Const Int
>              | Var   Id
>              | Add   r r
>              | Mul   r r
>              | IfNeg r r r
>                deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

> instance Eq1 ExprF where
>   liftEq _  (Const il)       (Const ir)       = il == ir
>   liftEq _  (Const  _)        _               = False
>   liftEq _           _       (Const  _)       = False
>   liftEq _  (Var il)         (Var ir)         = il == ir
>   liftEq _  (Var  _)          _               = False
>   liftEq _           _       (Var  _)         = False
>   liftEq eq (Add l1 l2)      (Add r1 r2)      = eq l1 r1 && eq l2 r2
>   liftEq _  (Add  _  _)       _               = False
>   liftEq _           _       (Add  _  _)      = False
>   liftEq eq (Mul l1 l2)      (Mul r1 r2)      = eq l1 r1 && eq l2 r2
>   liftEq _  (Mul  _  _)       _               = False
>   liftEq _                _  (Mul  _  _)      = False
>   liftEq eq (IfNeg l1 l2 l3) (IfNeg r1 r2 r3) = eq l1 r1 && eq l2 r2 && eq l3 r3

> -- TODO
> instance Show1 ExprF where
>   liftShowsPrec _  _ _  (Const _)     = showString "Const"
>   liftShowsPrec _  _ _  (Var   _)     = showString "Var"
>   liftShowsPrec _  _ _  (Add   _ _)   = showString "Add"
>   liftShowsPrec _  _ _  (Mul   _ _)   = showString "Mul"
>   liftShowsPrec _  _ _  (IfNeg _ _ _) = showString "IfNeg"

> type Id = String

> type Expr = Fix ExprF

*pattern functor* `ExprF` represents structure of `Expr`

isomorphism between data-type and its pattern functor type handled by `Fix` and `unFix`

example: evaluator with global environment
------------------------------------------

> type Env = Map Id Int

> eval :: Env -> Expr -> Maybe Int
> eval env = cata (evalAlg env)

> evalAlg :: Env -> ExprF (Maybe Int) -> Maybe Int
> evalAlg env = alg where
>   alg (Const c)     = pure c
>   alg (Var i)       = M.lookup i env
>   alg (Add x y)     = (+) <$> x <*> y
>   alg (Mul x y)     = (*) <$> x <*> y
>   alg (IfNeg t x y) = t >>= bool x y . (<0)

----

> e1 :: Fix ExprF
> e1 = Fix (Mul
>            (Fix (IfNeg
>                   (Fix (Mul (Fix (Const 1))
>                             (Fix (Var "a"))))
>                   (Fix (Add (Fix (Var "b"))
>                             (Fix (Const 0))))
>                   (Fix (Add (Fix (Var "b"))
>                             (Fix (Const 2))))))
>                 (Fix (Const 3)))

----

an example expression
---------------------

\vspace{0.2in}
\centerline{\hbox{%
\begin{tikzpicture}[]
\Tree [.Mul [.IfNeg [.Mul [.Const 1 ] [.Var a ] ] [.Add [.Var b ] [.Const 0 ] ] [.Add [.Var b ] [.Const 2 ] ] ] [.Const 3 ] ]
\end{tikzpicture}
}}

----

> testEnv :: Env
> testEnv = M.fromList [("a",1),("b",3)]

> ee1 = U.t "ee1"
>       (eval testEnv e1)
>       (Just 9)

----

example: collecting free variables
----------------------------------

> freeVars :: Expr -> S.Set Id
> freeVars = cata alg where
>     alg :: ExprF (S.Set Id) -> S.Set Id
>     alg (Var i) = S.singleton i
>     alg e       = fold e

> fve1 = U.t "fve1"
>        (freeVars e1)
>        (S.fromList ["a","b"])

----

example: substituting variables
-------------------------------

> substitute :: Map Id Expr -> Expr -> Expr
> substitute env = cata alg where
>   alg :: ExprF Expr -> Expr
>   alg e@(Var i) = fromMaybe (Fix e) $ M.lookup i env
>   alg e         = Fix e

> sub = M.fromList [("b",Fix $ Var "a")]
> svfe1 = U.t "svfe1"
>         (freeVars $ substitute sub e1)
>         (S.fromList ["a"])

Composing Algebras
==================

- in general, catamorphisms do not compose
- useful special case

example: an optimisation pipeline
---------------------------------

> optAdd :: ExprF Expr -> Expr
> optAdd (Add   (Fix (Const 0)) e) = e
> optAdd (Add e (Fix (Const 0))  ) = e
> optAdd                        e  = Fix e
>
> optMul :: ExprF Expr -> Expr
> optMul (Mul   (Fix (Const 1)) e) = e
> optMul (Mul e (Fix (Const 1))  ) = e
> optMul                        e  = Fix e

----

this composition uses two traversals:

> optimizeSlow :: Expr -> Expr
> optimizeSlow = cata optAdd . cata optMul

> os = U.t "os"
>      (optimizeSlow e1)
>      (Fix (Mul (Fix (IfNeg (Fix (Var "a"))
>                      (Fix (Var "b"))
>                      (Fix (Add (Fix (Var "b"))
>                            (Fix (Const 2))))))
>            (Fix (Const 3))))

An algebra composition operator is needed to enable *short-cut fusion*:

~~~{.haskell}
cata f . cata g = cata (f `comp` g)
~~~

For the special case:

~~~{.haskell}
f :: f a -> a;  g :: g (Fix f) -> Fix f
~~~~

for arbitrary functors `f` and `g`, this is: \
`comp x y = x . unFix . y`

----

more efficient optimized pipeline:\footnote{In practice, such a pipeline is likely to be iterated until an equality fixpoint is reached, hence efficiency is important.}

> optimizeFast :: Expr -> Expr
> optimizeFast = cata (optMul . unFix . optAdd)

> opf = U.t "opf"
>       (optimizeSlow e1)
>       (optimizeFast e1)

above applies *catamorphism compose law* [3]:

~~~{.haskell}
f :: f a -> a
h :: g a -> f a

cata f . cata (Fix . h) = cata (f . h)
~~~


Combining Algebras
==================

Algebras over same functor but different carrier types combined as products

- now two or more catamorphisms performed as one

Given two algebras,

~~~{.haskell}
f :: f a -> a;  g :: f b -> b
~~~

want algebra of type `f (a, b) -> (a, b)`

- use the *banana-split theorem* [3]:

~~~{.haskell}
cata f &&& cata g =
    cata ( f . fmap fst &&&
           g . fmap snd )
~~~

- Uses fan-out or _fork_ \footnote{defined more generally in Control.Arrow}

~~~{.haskell}
(&&&) :: (b -> c) -> (b -> d) -> b -> (c, d)
(f &&& g) x = (f x, g x)
~~~

\begin{picture}(0,0)(0,0)
\put(225,20){\includegraphics[height=0.8in]{images/banana-split.png}}
\end{picture}

----

- rewrite product using `funzip` (unzip for functors)

> algProd :: Functor f =>
>            (f a -> a) -> (f b -> b) ->
>            f (a, b) -> (a, b)
> algProd f g = (f *** g) . funzip

> funzip :: Functor f => f (a, b) -> (f a, f b)
> funzip = fmap fst &&& fmap snd

TODO: example usage

----

- or, combine two algebras over different functors but same carrier type into a coproduct

> algCoprod :: (f a -> a) -> (g a -> a) ->
>              Either (f a) (g a) -> a
> algCoprod = (|||)

- Uses fan-in \footnote{defined more generally in Control.Arrow}

~~~{.haskell}
(|||) ::: (b -> d) -> (c -> d) -> Either b c -> d
(|||) = either
~~~

TODO: example usage

----

Compositional Data-types
========================

- Unfixed types can be composed in a modular fashion
- REF: *Data types a la carte* [4]

> -- | coproduct of pattern functors f and g
> data (f :+: g) r = Inl (f r) | Inr (g r)

> -- | product of pattern functors f and g
> data (f :*: g) r = (f r) :*: (g r)

> -- | free monad pattern functor
> data FreeF f a r = FreeF (f r) | Pure a

> -- | cofree comonad pattern functor
> data CofreeF f a r = CofreeF (f r) a

\begin{picture}(0,0)(0,0)
\put(240,10){\includegraphics[height=1in]{images/jigsaw.jpg}}
\end{picture}

----

example: Templating
-------------------

- type-safe templating requires a syntax tree with holes
- parse a string template into tree, then fill holes

Use a *free monad* structure `Ctx f a` to represent a node with
either a term of type `f` or a hole of type `a`.

> -- | A Context is a term (f r) which can contain holes a
> data CtxF f a r = Term (f r) | Hole a
>                   deriving (Functor, Show)

> -- | Context fixed-point type. A free monad.
> type Ctx f a = Fix (CtxF f a)


\begin{picture}(0,0)(0,0)
\put(250,-15){\includegraphics[height=0.8in]{images/cookie-cutter.jpg}}
\end{picture}

----

Fill all the holes of type `a` in the template `Ctx f a` using the supplied function of type `a -> Fix f`

> fillHoles :: forall f a. Functor f =>
>              (a -> Fix f) -> Ctx f a -> Fix f
> fillHoles g = cata alg where
>   alg :: CtxF f a (Fix f) -> Fix f
>   alg (Term t) = Fix t
>   alg (Hole a) = g a

----

example: add template variables to JSON by composing data types and parsers.

- need an "unfixed" JSON datatype and parser (see appendix)

~~~{.haskell}
pJSValueF :: CharParser () r ->
             CharParser () (JSValueF r)
~~~

> pJSValue :: CharParser () JSValue
> pJSValue = fix $ \p -> Fix <$> pJSValueF p

- compose a new `JSTemplate` type

> type Name = String
> type JSTemplate = Ctx JSValueF Name

----

- define a parser for variable syntax: `${name}`

> pVar :: CharParser () Name
> pVar = char '$' *> between (char '{') (char '}')
>                      (many alphaNum)

- compose the variable parser with the unfixed JSON parser

> pJSTemplate :: CharParser () (Ctx JSValueF Name)
> pJSTemplate = fix $ \p ->
>   Fix <$> (Term <$> pJSValueF p <|> Hole <$> pVar)

----

> temp1 :: Ctx JSValueF Name
> temp1 = parse' pJSTemplate "[{\"foo\":${a}}]"

~~~
 > temp1
 Fix {unFix = Term (
   JSArray [Fix {unFix = Term (
     JSObject [("foo",Fix {unFix = Hole "a"})])}])})
~~~

> vlookup :: Ord a => Map a JSValue -> a -> JSValue
> vlookup env = fromMaybe (Fix JSNull) . (`M.lookup` env)

~~~
 > let env = M.fromList [("a", Fix $ JSNumber 42)]
 > fillHoles (vlookup env) temp1
 Fix {unFix =
   JSArray [Fix {unFix =
     JSObject [("foo",Fix {unFix = JSNumber 42.0})]}]}
~~~

----

example: Annotating
-------------------

- useful for storing intermediate values
- inspired by ideas from *attribute grammars*

Use a *cofree comonad* structure `Ann f a` to annotate nodes of
type `f` with attributes of type `a`.

> -- | Annotate (f r) with attribute a
> newtype AnnF f a r = AnnF (f r, a) deriving Functor

> -- | Annotated fixed-point type. A cofree comonad.
> type Ann f a = Fix (AnnF f a)

> -- | Attribute of the root node
> attr :: Ann f a -> a
> attr (unFix -> AnnF (_, a)) = a

\begin{picture}(0,0)(0,0)
\put(225,10){\includegraphics[height=1in]{images/post-it-note.jpg}}
\end{picture}

----

> -- | strip attribute from root
> strip :: Ann f a -> f (Ann f a)
> strip (unFix -> AnnF (x, _)) = x

> -- | strip all attributes
> stripAll :: Functor f => Ann f a -> Fix f
> stripAll = cata alg where
>   alg (AnnF (x, _)) = Fix x

> -- | annotation constructor
> ann :: (f (Ann f a), a) -> Ann f a
> ann = Fix . AnnF

> -- | annotation deconstructor
> unAnn :: Ann f a -> (f (Ann f a), a)
> unAnn (unFix -> AnnF a) = a

----

*Synthesized* attributes are created in a bottom-up traversal using a catamorphism.

> synthesize :: forall f a. Functor f =>
>               (f a -> a) -> Fix f -> Ann f a
> synthesize f = cata alg where
>   alg :: f (Ann f a) -> Ann f a
>   alg = ann . (id &&& f . fmap attr)

For example, annotating each node with the sizes of all subtrees:

> sizes :: (Functor f, Foldable f) => Fix f -> Ann f Int
> sizes = synthesize $ (+1) . sum

----

annotated with sizes
--------------------

\vspace{0.2in}
\centerline{\resizebox{4.5in}{!}{%
\begin{tikzpicture}[]
\Tree [.@ [.Mul [.@ [.IfNeg [.@ [.Mul [.@ [.Const 1 ] 1 ] [.@ [.Var a ] 1 ] ] 3 ] [.@ [.Add [.@ [.Var b ] 1 ] [.@ [.Const 0 ] 1 ] ] 3 ] [.@ [.Add [.@ [.Var b ] 1 ] [.@ [.Const 2 ] 1 ] ] 3 ] ] 10 ] [.@ [.Const 3 ] 1 ] ] 12 ]
\end{tikzpicture}
}}

----

*Inherited* attributes are created in a top-down manner from an initial value.

- Can still use a cata/paramorphism by using a higher-order carrier
- the bottom-up traversal happens top-down when the built function is run

> inherit :: forall f a. Functor f =>
>             (Fix f -> a -> a) -> a -> Fix f -> Ann f a
> inherit f root n0 = para alg n0 root where
>   alg :: f (a -> Ann f a, Fix f) -> a -> Ann f a
>   alg (funzip -> (ff, n)) p = ann (n', a)
>     where
>       a  = f (Fix n) p
>       n' = fmap ($ a) ff

----

For example, the `depths` function computes the depth of all subtrees:

> depths :: Functor f => Fix f -> Ann f Int
> depths = inherit (const (+1)) 0

~~~
 > pprAnn $ depths e1
 ((ifNeg (1 @ 4 * a @ 4) @ 3
    then (b @ 4 + 0 @ 4) @ 3
    else (b @ 4 + 2 @ 4) @ 3) @ 2
      * 3 @ 2) @ 1
~~~

Note: could combine the `synthesize` and `inherit` algebras and do both in one traversal.

----

annotated with depths
---------------------

\vspace{0.2in}
\centerline{\resizebox{4.5in}{!}{%
\begin{tikzpicture}[]
\Tree [.@ [.Mul [.@ [.IfNeg [.@ [.Mul [.@ [.Const 1 ] 4 ] [.@ [.Var a ] 4 ] ] 3 ] [.@ [.Add [.@ [.Var b ] 4 ] [.@ [.Const 0 ] 4 ] ] 3 ] [.@ [.Add [.@ [.Var b ] 4 ] [.@ [.Const 2 ] 4 ] ] 3 ] ] 2 ] [.@ [.Const 3 ] 2 ] ] 1 ]
\end{tikzpicture}
}}


Monadic variants
================

A monadic carrier type `m a` gives an algebra `f (m a) -> m a`

This is inconvenient, would have to explicitly sequence the embedded monadic values of the argument.

Define a variant combinator `cataM` that enables using an algebra with a monadic codomain only `f a -> m a`

- sequencing is done automatically by using `mapM` instead of `fmap`
- composition with the algebra must now happen in the Kleisli category

> cataM :: (Monad m, Traversable f) =>
>          (f a -> m a) -> Fix f -> m a
> cataM algM = algM <=< (mapM (cataM algM) . unFix)

----

example: eval revisited
-----------------------

- `cataM` simplifies working with a monadic algebra carrier types\footnote{compare and contrast the `IfNeg` clause between eval and eval'}
- monad transformers can offer much additional functionality, such as error handling

> eval' :: Env -> Expr -> Maybe Int
> eval' env = (`runReaderT` env) . cataM algM where
>     algM :: ExprF Int -> ReaderT Env Maybe Int
>     algM (Const c)     = return c
>     algM (Var i)       = ask >>= lift . M.lookup i
>     algM (Add x y)     = return $ x + y
>     algM (Mul x y)     = return $ x * y
>     algM (IfNeg t x y) = return $ bool x y (t<0)
>

NB. ReaderT would be especially useful for local environments.


Memoization
===========

- memoization, or caching: trade space for time
- since recursion restricted to a library of standard combinators, can define memoizing variants that can easily be swapped in

- the simplest (pure) memoize function requires some kind of `Enumerable` context

~~~{.haskell}
memoize :: Enumerable k => (k -> v) -> k -> v
~~~

----

- a monadic codomain enables using e.g. an underlying State or ST monad

> memoize :: Memo k v m => (k -> m v) -> k -> m v
> memoize f x = RS.lookup x >>= (`maybe` return)
>   (f x >>= \r -> RS.insert x r >> return r)

> memoFix :: Memo k v m =>
>            ((k -> m v) -> k -> m v) -> k -> m v
> memoFix f = let mf = memoize (f mf) in mf

----

- runs the memoized computation using a HashTable (see appendix for `Memo` instance)

> runMemo ::
>   (forall s. ReaderT (C.HashTable s k v) (ST s) a) -> a
> runMemo m = runST $ H.new >>= runReaderT m

- a (transparent) memoizing catamorphism

> {-
> memoCata :: (Eq (f (Fix f)), Traversable f,
>             Hashable (Fix f)) =>
>             (f a -> a) -> Fix f -> a
> memoCata f x = runMemo $
>   memoFix (\rec -> fmap f . mapM rec . unFix) x
> -}

**WARNING** this could result in a slowdown unless your algebra is significantly more expensive than a hash computation!


Appendix
========

memo monad class and HashTable instance
----------------------------------------

> class Monad m => Memo k v m | m -> k, m -> v where
>   lookup  :: k -> m (Maybe v)
>   insert  :: k -> v -> m ()

> -- | HashTable-based Memo monad
> instance (Eq k, Hashable k, HashTable h) =>
>          Memo k v (ReaderT (h s k v) (ST s)) where
>   lookup k   = ask >>= \h -> lift $ H.lookup h k
>   insert k v = ask >>= \h -> lift $ H.insert h k v

----

Expr Hashable instance
----------------------

> instance Hashable Expr where
>   hashWithSalt s = foldl hashWithSalt s . unFix

> instance Hashable r => Hashable (ExprF r) where
>   hashWithSalt s (Const c)
>     = 1 `hashWithSalt` s `hashWithSalt` c
>   hashWithSalt s (Var ident)
>     = 2 `hashWithSalt` s `hashWithSalt` ident
>   hashWithSalt s (Add x y)
>     = 3 `hashWithSalt` s `hashWithSalt` (x, y)
>   hashWithSalt s (Mul x y)
>     = 4 `hashWithSalt` s `hashWithSalt` (x, y)
>   hashWithSalt s (IfNeg t x y)
>     = 5 `hashWithSalt` s `hashWithSalt` (t, x, y)

----

unfixed JSON data-type
----------------------

> data JSValueF r
>     = JSNull
>     | JSBool     Bool
>     | JSNumber   Double
>     | JSString   String
>     | JSArray    [r]
>     | JSObject   [(String, r)]
>     deriving (Eq, Foldable, Functor, Ord, Show)
>
> type JSValue = Fix JSValueF

----

simple unfixed JSON parser
--------------------------

- Modified from code published in _Real World Haskell_

> parse' :: CharParser () a -> String -> a
> parse' p = either (error . show) id . parse p "(unknown)"

> pJSValueF :: CharParser () r ->
>              CharParser () (JSValueF r)
> pJSValueF r = spaces *> pValue r

> pSeries :: Char -> CharParser () r ->
>            Char -> CharParser () [r]
> pSeries left parser right =
>     between (char left <* spaces) (char right) $
>             (parser <* spaces) `sepBy`
>                 (char ',' <* spaces)

----

> pArray :: CharParser () r -> CharParser () [r]
> pArray r = pSeries '[' r ']'

> pObject :: CharParser () r -> CharParser () [(String, r)]
> pObject r = pSeries '{' pField '}'
>     where pField = (,) <$>
>             (pString <* char ':' <* spaces) <*> r

> pBool :: CharParser () Bool
> pBool = True <$ string "true"
>      <|> False <$ string "false"

----

> pValue :: CharParser () r -> CharParser () (JSValueF r)
> pValue r = value <* spaces
>   where value = choice [ JSString <$> pString
>                        , JSNumber <$> pNumber
>                        , JSObject <$> pObject r
>                        , JSArray  <$> pArray r
>                        , JSBool   <$> pBool
>                        , JSNull   <$  string "null"
>                        ]
>                 <?> "JSON value"

----

> pNumber :: CharParser () Double
> pNumber = do s <- getInput
>              case readSigned readFloat s of
>                [(n, s')] -> n <$ setInput s'
>                _         -> empty

> pString :: CharParser () String
> pString = between (char '\"') (char '\"') (many jchar)
>     where jchar = char '\\' *> pEscape
>               <|> satisfy (`notElem` "\"\\")

> pEscape :: ParsecT String u DFI.Identity Char
> pEscape = choice (zipWith decode
>                   "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
>     where decode c r = r <$ char c

----

> data LTreeF a r = Leaf a | Bin r r

LTreeF functor instance
-----------------------

> instance Functor (LTreeF a) where
>   fmap _ (Leaf a)    = Leaf a
>   fmap f (Bin r1 r2) = Bin (f r1) (f r2)
>

tikz-qtree printer for leaf trees
---------------------------------

> pQtLTree :: Pretty a => Fix (LTreeF a) -> Doc
> pQtLTree = (text "\\Tree" <+>) . cata alg where
>   alg (Leaf a)  = node ".Leaf"$ pretty a
>   alg (Bin l r) = node ".Bin" $ l <+> r

----

a tikz-qtree printer
--------------------

> pQt :: Expr -> Doc
> pQt = (text "\\Tree" <+>) . cata pQtAlg

> pQtAlg :: ExprF Doc -> Doc
> pQtAlg (Const c)     = node ".Const" $ text $ show c
> pQtAlg (Var ident)   = node ".Var"   $ text ident
> pQtAlg (Add x y)     = node ".Add"   $ x <+> y
> pQtAlg (Mul x y)     = node ".Mul"   $ x <+> y
> pQtAlg (IfNeg t x y) = node ".IfNeg" $ t <+> x <+> y
>
> node :: String -> Doc -> Doc
> node s d = PP.brackets $ text s <+> d PP.<> space

----

tikz-qtree printer for annotated trees
---------------------------------------

> pQtAnn :: Pretty a => Ann ExprF a -> Doc
> pQtAnn = (text "\\Tree" <+>) . cata alg where
>   alg (AnnF (d, a)) = node ".@" $ pQtAlg d <+> pretty a

----

> testRS :: IO Counts
> testRS =
>     runTestTT $ TestList $ f2fold ++ f1foldMap ++ ifi ++ ofi ++ f1foldr ++ f1foldr2 ++
>                            sc ++ ni ++ ni2 ++ fi ++ fpl ++ lx ++ fxl ++ ee1 ++ fve1 ++ svfe1 ++
>                            os ++ opf ++ rep ++ lb ++ itn ++ ml ++ ts  ++ mst ++ fct ++
>                            infz ++ infs ++ otfz ++ otfs ++ factEqR ++
>                            np ++ tl ++ sl ++ sl2 ++ ie ++ iss ++ fve2 ++ ofe2 ++ di ++ fibt ++ ev ++
>                            exs

\iffalse

> matchAll :: String -> a
> matchAll msg = error (msg ++ " match all pattern")

\fi

References
==========

[1] J. Gibbons, "Origami programming.", The Fun of Programming, Palgrave, 2003.

[2] C. McBride & R. Paterson, "Applicative programming with effects", Journal of Functional Programming, vol. 18, no. 01, pp. 1-13, 2008.

[3] E. Meijer, "Functional Programming with Bananas , Lenses , Envelopes and Barbed Wire", 1991.

[4] W. Swierstra, "Data types a la carte", Journal of Functional Programming, vol 18, no. 04, pp. 423-435, Mar. 2008.

----

[5] L. Augusteijn, "Sorting morphisms" pp. 1-23, 3rd International Summer School on Advanced Functional Programming, volume 1608 of LNCS, 1998.

[6] V. Vene, "Functional Programming with Apomorphisms (Corecursion)" pp. 147-161, 1988.

[7] T. Uustalu & V. Venu, "Primitive (Co)Recursion and Course-of-Value (Co)Iteration, Categorically" Informatica, Vol. 10, No. 1, 5-26, 1999.

Tim Williams's recursion schemes presentation

- http://www.timphilipwilliams.com/slides.html
- https://www.youtube.com/watch?v=Zw9KeP3OzpU


[FD] https://wiki.haskell.org/Functional_dependencies

------------------------------------------------------------------------------
-- TODO : from NatF.lhs

> instance Eq1 NatF where
>   liftEq _   ZeroF     ZeroF    = True
>   liftEq _   ZeroF    (SuccF _) = False
>   liftEq _  (SuccF _)  ZeroF    = False
>   liftEq eq (SuccF l) (SuccF r) = eq l r

> instance Show1 NatF where
>   liftShowsPrec _  _ _  ZeroF    = showString "ZeroF"
>   liftShowsPrec sp _ d (SuccF n) = showsUnaryWith sp "SuccF" d n

------------------------------------------------------------------------------
-- TODO : from ListF.lhs

> instance Eq a => Eq1 (ListF a) where
>   liftEq _   N          N        = True
>   liftEq _  (C _ _)     N        = False
>   liftEq _   N         (C _   _) = False
>   liftEq eq (C al rl)  (C ar rr) = al == ar && eq rl rr

> instance Show1 (ListF a) where
>   liftShowsPrec _  _ _  N      = showString "N"
>   liftShowsPrec sp _ d (C _a r) = showsUnaryWith sp "C" d r -- TODO missing a
