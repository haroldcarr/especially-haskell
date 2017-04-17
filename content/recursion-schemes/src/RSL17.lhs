Refactoring Recursion
=====================

\begin{center}

\LARGE{Harold Carr}

\end{center}

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
- use "library" functions to recursivly operate on recursive data
    - "folds" (aka "catamorphism")
    - "unfolds" (aka "anamorphism")
    - unfolds followed by folds (aka "hylomorphism")
- conclusion

- not covered
    - other recursion schemes
    - explicit recursive data
    - factor recursion out of recursive data with `Fix`

----

\newcommand{\ignore1}[1]{}

\ignore1{

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
> module RSL17 where
>
> import           Control.Arrow         (second)
> import           Prelude               as P hiding (replicate, succ)
>
> -- Third-party Hackage packages
>
> import           Test.HUnit                    (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util               as U (t, tt)
>
> {-# ANN module "HLint: ignore Use foldr" #-}
> {-# ANN module "HLint: ignore Use sum"   #-}
> {-# ANN module "HLint: ignore Use and"   #-}

}

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

\newcommand{\ignore2}[1]{}

\ignore2{

> sumF :: (Foldable t, Num b) => t b    -> b

}

> sumF  = foldr (+)  0

\newcommand{\ignore3}[1]{}

\ignore3{

> andF :: Foldable t          => t Bool -> Bool

}

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

\newcommand{\ignore4}[1]{}

\ignore4{

> lengthF :: (Foldable t, Num b) => t a -> b

}

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
- inductive recursion : each recursive step consumes one or more constructors
- ensures terminates (if given finite input)

----

> cataL :: (a ->        b -> b) -> b -> [a] -> b
> cataL f b (a : as) = f a    (cataL f b as)
> cataL _ b      []  = b

> c1 = U.t "c1" (cataL ((++) . show) "" [1,2,3::Int]) "123"

----

> filterL  :: (a -> Bool) -> [a] -> [a]
> filterL p  = cataL (\x acc -> if p x then x : acc else acc) []

> filterL' :: (a -> Bool) -> [a] -> [a]
> filterL' p = cataL (\x -> if p x then (x :) else id) []

> c2 = U.t "c2"  (filterL  odd [1,2,3::Int]) [1,3]
> c2'= U.t "c2'" (filterL' odd [1,2,3::Int]) [1,3]

----

TODO

other examples:

 http://www.cantab.net/users/antoni.diller/haskell/units/unit06.html

------------------------------------------------------------------------------

Anamorphisms
============

*ana* meaning *upwards* : generalized unfold

- corecursive dual of catamorphisms
- produces streams and other regular structures from a seed
- `ana` for lists is `unfoldr` (`ViewPatterns` help see the duality)

Corecursion

- co-inductive co-recursion : each recursive step guarded by a constructor
- although result can be infinite, each step (a constructor) is produced in finite time (i.e., makes progress)

> anaL  :: (b ->       (a, b))               -> b -> [a]
> anaL  f b = let (a, b') = f b in a : anaL f b'

> anaL' :: (b -> Maybe (a, b))               -> b -> [a]
> anaL' f b = case f b of
>    Just (a, b') -> a : anaL' f b'
>    Nothing      -> []

> unfoldr :: (b -> Maybe (a, b)) ->  b -> [a]
> unfoldr f (f -> Nothing)                   = []
> unfoldr f (f -> Just (x, unfoldr f -> xs)) = x : xs

\newcommand{\ignore5}[1]{}

\ignore5 {

> unfoldr _ _ = matchAll "unfoldr"

}

~~~{.haskell}
foldrP  :: (Maybe (a, b) -> b) -> [a] -> b
foldrP f []     = f Nothing
foldrP f (x:xs) = f (Just (x, foldrP f xs))
~~~

----

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
>   c ([]  ,   [])          = Nothing
>   c ([]  , y:ys)          = Just (y, (  [],   ys))
>   c (x:xs,   [])          = Just (x, (  xs,   []))
>   c (x:xs, y:ys) | x <= y = Just (x, (  xs, y:ys))
>                  | x >  y = Just (y, (x:xs,   ys))
>   c (_:_ , _:_)           = matchAll "mergeLists"

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

example: coinductive streams
----------------------------

> -- generates infinite stream
> iterateS :: (a -> a) -> a -> [a]
> iterateS f = anaL c where
>   c x = (x, f x)

> sFrom1 :: [Integer]
> sFrom1 = iterateS (+1) 1

> s1s :: [Integer]
> s1s = iterateS (id) 1

> takeS :: Int -> [a] -> [a]
> takeS 0     _  = []
> takeS _    []  = []
> takeS n (x:xs) = x : takeS (n-1) xs

> tsf = U.t "tsf"
>       (takeS 6 sFrom1)
>       [1,2,3,4,5,6]

> tss = U.t "tss"
>       (takeS 6 s1s)
>       [1,1,1,1,1,1]


------------------------------------------------------------------------------

hylomorphism
============

> hyloL :: (a -> c -> c) -> c -> (b -> Maybe (a, b)) -> b -> c
> hyloL f z g = cataL f z . anaL' g

> -- fusion/deforestation
> hyloL':: (a -> c -> c) -> c -> (c -> Maybe (a, c)) -> c
> hyloL' f z g = case g z of
>   Nothing     -> z
>   Just (x,z') -> f x (hyloL' f z' g)

> hl :: (Integral a, Show a) => [a] -> [(a, a)]
> hl b = hyloL f [] g b
>  where
>   g      []   = Nothing
>   g  (x:xs)   = Just ((x,x*2), xs)
>   f a@(l,_) c = if even l then a : c else c

> hle = U.t "hle"
>       (hl [1,2,3,4,5::Int])
>       [(2,4),(4,8)]

------------------------------------------------------------------------------

Paramorphisms
=============

*para* meaning *beside* : extension of catamorphism

- enables access to the original input structures
- provides access to input arg corresponding to running state of the recursion

> paraL :: (a -> [a] -> b -> b) -> b -> [a] -> b
> paraL f b (a : as) = f a as (paraL f b as)
> paraL _ b []       = b

> paraL':: (     [a] -> b -> b) -> b -> [a] -> b
> paraL' f b as@(_:xs) = f as (paraL' f b xs)
> paraL' _ b []        = b

> tails :: [a] -> [[a]]
> tails = paraL (\_ as b -> as:b) []

> p1 :: [Test]
> p1 = U.t "p1"
>      (tails [1,2,3,4::Int])
>      [[2,3,4],[3,4],[4],[]]


> slide :: Int -> [a] -> [[a]]
> slide n = paraL alg [] where
>   alg _ [] b                     = b
>   alg a as b | length (a:as) < n = b
>              | otherwise         = take n (a:as) : b

sliding n = para alg where
  alg N             = []
  alg (C x (r, xs)) = take n (x:xs) : r

NB. lookahead via input arg is left-to-right, but input list processed from the right

sl = U.t "sl"
     (sliding 3 [1..5::Int])
     [[1,2,3],[2,3,4],[3,4,5],[4,5],[5]]

> slide' :: Int -> [a] -> [[a]]
> slide' n = paraL' alg [] where
>   alg [] b                 = b
>   alg as b | length as < n = b
>            | otherwise     = take n as : b

example: slideing window 2

sliding2 :: Int -> [a] -> [[a]]
sliding2 n = para alg where
  alg N             = []
  alg (C x (r, xs)) | length (x:xs) < n = []
                    | otherwise         = take n (x:xs) : r

NB. lookahead via input arg is left-to-right, but input list processed from the right

sl2 = U.t "sl2"
      (sliding2 3 [1..5::Int])
      [[1,2,3],[2,3,4],[3,4,5]]

 http://stackoverflow.com/a/13317563/814846

> p2 :: [Test]
> p2 = U.tt "p2"
>      [ slide  3 [1..6::Int]
>      , slide' 3 [1..6::Int]
>      ]
>      [[1,2,3],[2,3,4],[3,4,5],[4,5,6]]

------------------------------------------------------------------------------

apomorphism
===========

> apoL  :: (b -> Maybe (a, b)) -> (b -> [a]) -> b -> [a]
> apoL f h b = case f b of
>   Just (a, b') -> a : apoL f h b'
>   Nothing      -> h b

> ap :: (Num a, Ord a) => [a] -> [a]
> ap = apoL f h
>  where
>   f    []  = Nothing
>   f (x:xs) = if x < 5 then Just (x*10,xs) else Nothing
>   h = id

> ae :: [Test]
> ae = U.t "ae"
>      (ap [1,3,5,7,9::Int])
>      [10,30,5,7,9]

------------------------------------------------------------------------------

> main :: IO Counts
> main =
>   runTestTT $ TestList $ c1 ++ c2 ++ c2' ++ rep ++ lb ++ ml ++ tsf ++ tss ++
>                          hle ++ p1 ++ p2 ++ ae

------------------------------------------------------------------------------

> matchAll :: String -> a
> matchAll msg = error (msg ++ " match all pattern")
