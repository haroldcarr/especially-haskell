Refactoring Recursion
=====================

\begin{center}

\LARGE{Harold Carr}

\end{center}

----

\textbf{introduction}
---------------------

- Recursion is a pattern
- There are different patterns of recursion
- "factoring" recursion : benefits
    - code/idea reuse
    - use "proven loops" --- less bugs
    - use catalogue of theorems to optimise/prove properties

----

\textbf{overview}
-----------------

- explicit recursive functions
- factor recursion out of functions with `fold`
- use library functions to operate lists
    - folds (aka "catamorphism")
    - unfolds (aka "anamorphism")
    - unfolds followed by folds (aka "hylomorphism")
    - TODO
- how to generalize to any recursive data
    - `Foldable`, `Traversable`, `Fix`

----

\setlength{\tabcolsep}{8pt}
\renewcommand{\arraystretch}{1.5}

\begin{tabular}{ l l p{6cm} }
\textbf{recursion} / \textbf{data}  &      & \textbf{corecursion} / \textbf{codata} \\
\hline
cata            &      & ana \\
                & hylo &  \\
para (cata++)   &      & apo (ana++) \\
histo           &      & futu \\
zygo (para gen) &      &  \\
\end{tabular}

\iffalse

> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults      #-}
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

\fi

----

\textbf{explicit recursion}
---------------------------

note the pattern

> sumE    []  = 0
> sumE (x:xs) = x +  sumE xs

> andE    []  = True
> andE (x:xs) = x && andE xs

same recursive structure, except

- `0` or `True` : base case (i.e., empty list)
- `+` or `&&`   : operator in inductive case

----

\textbf{factor recursion out of functions with `fold`}
------------------------------------------------------

\iffalse

> sumF :: (Foldable t, Num b) => t b    -> b
> andF :: Foldable t          => t Bool -> Bool

\fi

> sumF  = foldr (+)  0
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

> lengthE []     = 0
> lengthE (_:xs) = 1 + lengthE xs

\iffalse

> lengthF :: (Foldable t, Num b) => t a -> b

\fi

> lengthF        = foldr  (\_ n -> 1 + n)  0

~~~{.haskell}
lengthFL       = foldl' (const . P.succ) 0
~~~

\footnotesize

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

\normalsize

----

\textbf{`foldr` operation}

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

----

\textbf{recusion as library functions}
--------------------------------------

\begin{tabular}{ l l p{6cm} }
{\tt cata}  &    catamorphism  & folds \\
{\tt ana}   &    anamorphisms  & unfolds \\
{\tt hylo}  &    hylomorphism  & {\tt ana} then {\tt cata} \\
            &                  & (corecursive production followed by recursive consumption) \\
\end{tabular}


----

\textbf{catamorphisms}
----------------------

*cata* meaning *downwards* : aka `fold`

- models *iteration*
- inductive recursion
    - each recursive step consumes one or more constructors
- ensures terminates (if given finite input)

----

> cataL :: (a -> b -> b) -> b -> [a] -> b
> cataL f b (a : as) = f a (cataL f b as)
> cataL _ b      []  = b

> c1 = U.t "c1"
>     (cataL (+) 0 [1,2,3])
>     6

----

> filterL  :: (a -> Bool) -> [a] -> [a]
> filterL p  =
>   cataL (\x acc -> if p x then x : acc
>                           else acc)
>         []

> filterL' :: (a -> Bool) -> [a] -> [a]
> filterL' p = cataL alg [] where
>   alg x | p x       = (x :)
>         | otherwise = id

> c2 = U.tt "c2" [ (filterL  odd [1,2,3])
>                , (filterL' odd [1,2,3])
>                ]
>                [1,3]

----

TODO

other examples:

 http://www.cantab.net/users/antoni.diller/haskell/units/unit06.html

------------------------------------------------------------------------------

\textbf{anamorphisms}
---------------------

*ana* meaning *upwards* : aka `unfold`

- corecursive dual of catamorphisms
- produces structures from a seed
- corecursion produces (infinite?) *codata*
- recursion consumes (finite) *data*

> anaL  :: (b ->       (a, b)) -> b -> [a]
> anaL  f b = let (a, b') = f b in a:anaL f b'

> anaL' :: (b -> Maybe (a, b)) -> b -> [a]
> anaL' f b = case f b of
>               Just (a, b') ->   a:anaL' f b'
>               Nothing      -> []

----

> replicate :: Int -> a -> [a]
> replicate n0 x = anaL' c n0 where
>   c 0 = Nothing
>   c n = Just (x, n-1)

> rep = U.t "rep" (replicate 4 '*') "****"

----

> fibs :: [Integer]
> fibs = anaL' (\(a,b) -> Just (a,(b,a+b)))
>              (0,1)

> fib = U.t "tf" (fibs !! 7) 13

----

> linesBy :: (t -> Bool) -> [t] -> [[t]]
> linesBy p = anaL' c where
>   c []  = Nothing
>   c xs  = Just $ second (drop 1) $ break p xs

> lb = U.t "lb"
>      (linesBy (==',') "foo,bar,baz")
>      ["foo","bar","baz"]

----

example: merging lists
----------------------

given 2 sorted, produce 1 sorted list

\small

> mergeLists :: forall a. Ord a => [a] -> [a] -> [a]
> mergeLists = curry $ anaL' c where
>   c :: Ord a => ([a], [a]) -> Maybe (a, ([a], [a]))
>   c (  [],   [])          = Nothing
>   c (  [], y:ys)          = Just (y, (  [],   ys))
>   c (x:xs,   [])          = Just (x, (  xs,   []))
>   c (x:xs, y:ys) | x <= y = Just (x, (  xs, y:ys))
>                  | x >  y = Just (y, (x:xs,   ys))
>   c (_:_ , _:_)           = matchAll "mergeLists"

> ml = U.t "ml"
>      (mergeLists [1,4] [2,3,5])
>      [1,2,3,4,5]

\normalsize

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

----

> takeS :: Int -> [a] -> [a]
> takeS 0     _  = []
> takeS _    []  = []
> takeS n (x:xs) = x : takeS (n-1) xs

> tsf = U.t "tsf"
>       (takeS 6 sFrom1)
>       [1,2,3,4,5,6]

------------------------------------------------------------------------------

\textbf{hylomorphism}
---------------------

composition of catamorphism and anamorphism

- corecursive codata production
- followed by recursive data consumption

> hyloL :: (a -> c -> c)       -> c
>       -> (b -> Maybe (a, b)) -> b
>       -> c
> hyloL f z g = cataL f z . anaL' g

----

> fact :: Integer -> Integer
> fact n0 = hyloL' c 1 a n0 where
>   a 0 = Nothing
>   a n = (Just (n, n - 1))
>   c   = (*)

> hf = U.t "hf" (fact 5) 120

----

\textbf{fusion/deforestation}

~~~{.haskell}
hyloL f z g = cataL f z . anaL' g
~~~

> hyloL' f a g = h where
>   h b = case g b of
>     Nothing       -> a
>     Just (a', b') -> f a' (h b')

------------------------------------------------------------------------------

Paramorphism
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
>      (tails [1,2,3,4])
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
     (sliding 3 [1..5])
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
      (sliding2 3 [1..5])
      [[1,2,3],[2,3,4],[3,4,5]]

 http://stackoverflow.com/a/13317563/814846

> p2 :: [Test]
> p2 = U.tt "p2"
>      [ slide  3 [1..6]
>      , slide' 3 [1..6]
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
>      (ap [1,3,5,7,9])
>      [10,30,5,7,9]

------------------------------------------------------------------------------

\iffalse

> testRSL17 :: IO Counts
> testRSL17 =
>   runTestTT $ TestList $ c1 ++ c2 ++ rep ++ fib ++ lb ++ ml ++ tsf ++
>                          p1 ++ p2 ++ ae

> matchAll :: String -> a
> matchAll msg = error (msg ++ " match all pattern")

\fi

References
==========

todo
