\iffalse

> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults      #-}
>
> {-# LANGUAGE FlexibleContexts       #-}
> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE RankNTypes             #-}
> {-# LANGUAGE ScopedTypeVariables    #-}
> {-# LANGUAGE TupleSections          #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE UndecidableInstances   #-}
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
> {-# ANN fact "HLint: ignore Eta reduce"  #-}

\fi

\begin{center}

\textbf{\LARGE{Refactoring Recursion}}

\Large{Harold Carr}

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
    - folds : "catamorphism"
    - unfolds : "anamorphism"
    - unfolds followed by folds : "hylomorphism"
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

> c2 = U.tt "c2" [ filterL  odd [1,2,3]
>                , filterL' odd [1,2,3]
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
> s1s = iterateS id 1

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
> hyloL f c g = cataL f c . anaL' g

----

> fact :: Integer -> Integer
> fact n0 = hyloL' c 1 a n0 where
>   a 0 = Nothing
>   a n = Just (n, n - 1)
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

\textbf{paramorphism}
---------------------

*para* meaning *beside* (or "parallel with") : extension of catamorphism

- given each element, and
- current cursor in iteration (e.g., the current tail)

> paraL  :: (a -> [a] -> b -> b) -> b
>        -> [a]
>        -> b
> paraL  f b  (a : as) = f a as (paraL f b as)
> paraL  _ b       []  = b

----

> tails :: [a] -> [[a]]
> tails = paraL (\_ as b -> as:b) []

> p1 = U.t "p1"
>      (tails [1,2,3,4])
>      [[2,3,4],[3,4],[4],[]]

----

> slide :: Int -> [a] -> [[a]]
> slide n = paraL alg [] where
>   alg _ [] b                     = b
>   alg a as b | length (a:as) < n = b
>              | otherwise = take n (a:as) : b

> sl = U.t "sl"
>      (slide 3 [1..5])
>      [[1,2,3],[2,3,4],[3,4,5]]

------------------------------------------------------------------------------

\textbf{apomorphism}
--------------------

*apo* meaning *apart*

- dual of paramorphism
- extension anamorphism
- enables short-circuiting traversal

> apoL :: ([b] -> Maybe (a, Either [b] [a]))
>      -> [b]
>      -> [a]
> apoL f bs = case f bs of
>   Nothing -> []
>   Just (a, Left  bs') -> a : apoL f bs'
>   Just (a, Right as)  -> a : as

----

- short-circuits to final result when `x<=y`

> insertElemL :: Ord a => a -> [a] -> [a]
> insertElemL a as = apoL c (a:as) where
>   c      []           = Nothing
>   c     [x]           = Just (x, Left     [])
>   c (x:y:xs) | x <= y = Just (x, Right (y:xs)) -- DONE
>              | x >  y = Just (y, Left  (x:xs))

\iffalse

>   c _                 = error "insertElem"

\fi

> iel = U.t "iel"
>      (insertElemL 3 [1,2,5])
>      [1,2,3,5]

----

\textbf{zygomorphism}
---------------------

- generalisation of paramorphism
- fold that depends on result of another fold
    - on each iteration of fold
    - f sees its  answer  from previous iteration
    - g sees both answers from previous iteration
    - fused into one traversal

\small

> zygoL :: (a -> b -> b)      -- f
>       -> (a -> b -> c -> c) -- g depends on f
>       -> b -> c             -- zeroes
>       -> [a]                -- input
>       -> c                  -- result
> zygoL f g b0 c0 =
>   snd . cataL (\a (b, c) -> (f a b, g a b c))
>               (b0, c0)

\normalsize

----

> pmL :: [Int] -> [Int]
> pmL = zygoL (\_ b -> not b)
>             (\a b c -> pm b a c)
>             True
>             []
>  where pm b a c = (if b then -a else a) : c

> zpm = U.t "zpm"
>       (pmL [1,2,3,4,5])
>       [-1,2,-3,4,-5]

----

> pmL':: [Int] -> [Int]
> pmL'= zygoL (\_ b -> b + 1)
>             (\a b c -> pm (b`mod`3==0) a c)
>             (-1)
>             []
>  where pm b a c = (if b then -a else a) : c

> zpm' = U.t "zpm'"
>        (pmL' [1,2,3,4,5,6,7])
>        [1,2,-3,4,5,-6,7]

------------------------------------------------------------------------------

\textbf{histomorphism}
----------------------

------------------------------------------------------------------------------

\textbf{futumorphism}
---------------------

- corecursive dual of histomorphism
    - histo : access to previously-computed values
    - futu  : access to future values

> futuL :: (a -> Maybe (b, ([b], Maybe a)))
>       -> a
>       -> [b]
> futuL f a =
>   case f a of
>     Nothing            -> []
>     Just (b, (bs, ma)) -> b : (bs ++ futuBs)
>       where futuBs = case ma of
>               Nothing -> []
>               Just a' -> futuL f a'

----

> exchL = futuL coa where
>   coa xs = Just ( head (tail xs),
>                   ( [head xs],
>                     Just (tail (tail xs))
>                   )
>                 )

> exs1 = U.t "exs"
>        (takeS 10 $ exchL sFrom1)
>        [2,1,4,3,6,5,8,7,10,9]

> exs2 = U.t "exs2"
>        (takeS  9 $ exchL sFrom1)
>        [2,1,4,3,6,5,8,7,10]

------------------------------------------------------------------------------

\iffalse

> testRSL17 :: IO Counts
> testRSL17 =
>   runTestTT $ TestList $ c1 ++ c2 ++ rep ++ fib ++ lb ++ ml ++ tsf ++ hf ++
>                          p1 ++ sl ++ iel ++ zpm ++ zpm' ++ exs1 ++ exs2

> matchAll :: String -> a
> matchAll msg = error (msg ++ " match all pattern")

\fi

References
==========

todo
