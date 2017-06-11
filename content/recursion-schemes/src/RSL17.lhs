\iffalse

> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults      #-}
>
> {-# LANGUAGE DeriveFoldable         #-}
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
> import           Data.Functor.Foldable (Fix (..))
> import           Fixpoint
> import           Prelude               as P hiding (replicate, succ)
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t, tt)
> import           TreeF                 hiding (et1, ext1)
>
> {-# ANN module "HLint: ignore Use foldr" #-}
> {-# ANN module "HLint: ignore Use sum"   #-}
> {-# ANN module "HLint: ignore Use and"   #-}
> {-# ANN hyloL' "HLint: ignore Eta reduce"  #-}
> {-# ANN fact "HLint: ignore Eta reduce"  #-}
> {-# ANN et1 "HLint: ignore Use concat"  #-}

\fi

\begin{center}
\textbf{\LARGE{Refactoring Recursion}}
\Large{Harold Carr}
\end{center}

\normalsize

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
- factor recursion out of \textbf{functions} with `fold`
- library functions to do recursion
    - folds : apply function to every element
    - unfolds : create structure from seed
    - unfolds followed by folds
    - (un)fold with early exit
    - ...
- factor recursion out of \textbf{data} with
    - `Foldable`, `Traversable`, `Fix`

----

\small

\setlength{\tabcolsep}{8pt}
\renewcommand{\arraystretch}{1.5}

\begin{tabular}{ l l p{6cm} }
\textbf{recursion} / \textbf{data}  & \textbf{both}     & \textbf{corecursion} / \textbf{codata} \\
\hline
cata                                &                   & ana \\
                                    & hylo              & \\
para (cata++)                       &                   & apo (ana++) \\
histo                               &                   & futu \\
zygo/mutu (para++)                  &                   & \\
\end{tabular}

\normalsize

------------------------------------------------------------------------------

\begin{center}

\textbf{\Large{refactoring recursion\\
out of functions}}
\end{center}

\normalsize

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

\small

\begin{tabular}{ l l p{6cm} }
{\tt cata}  &    catamorphism  & folds \\
{\tt ana}   &    anamorphisms  & unfolds \\
{\tt hylo}  &    hylomorphism  & {\tt ana} then {\tt cata} \\
{\tt para}  &    paramorphism  & {\tt cata} with access to cursor \\
{\tt apo}   &    apomorphism   & {\tt ana} with early exit \\
{\tt histo} &    histomorphism & {\tt cata} with access to prev values \\
{\tt futu}  &    futumorphism  & {\tt ana} with access to future values \\
{\tt zygo}  &    zygomorphism  & {\tt cata} with helper function \\
{\tt mutu}  &    mutumorphism  & {\tt cata} with helper function \\
\end{tabular}

\normalsize

----

\textbf{catamorphisms}
----------------------

*cata* meaning *downwards* : aka `fold`

- iteration

> cataL :: (a -> b -> b) -> b -> [a] -> b
> cataL f b (a : as) = f a (cataL f b as)
> cataL _ b      []  = b

> c1 = U.t "c1"
>     (cataL (+) 0 [1,2,3])
>     6

----

> cd = U.t "cd"
>      (cataL (\a b -> read a + b)
>             0.0
>             ["1.1", "2.2", "3.3"])
>      6.6

----

> filterL  :: (a -> Bool) -> [a] -> [a]
> filterL p  =
>   cataL (\a as -> if p a then a : as
>                          else     as)
>         []

\iffalse

> filterL' :: (a -> Bool) -> [a] -> [a]

\fi

> filterL' p = cataL alg [] where
>   alg a | p a       = (a :)
>         | otherwise = id

> c2 = U.tt "c2" [ filterL  odd [1,2,3]
>                , filterL' odd [1,2,3]
>                ] [1,3]

------------------------------------------------------------------------------

\textbf{anamorphism}
---------------------

*ana* meaning *upwards* : aka `unfold`

- corecursive dual of catamorphisms
- produces structures from a seed
- corecursion produces (infinite?) *codata*
    - (recursion consumes finite *data*)

> anaL  :: (b ->       (a, b)) -> b -> [a]
> anaL  f b = let (a, b') = f b in a:anaL f b'

> anaL' :: (b -> Maybe (a, b)) -> b -> [a]
> anaL' f b = case f b of
>               Just (a, b') ->   a:anaL' f b'
>               Nothing      -> []

----

> replicate :: Int -> a -> [a]
> replicate n0 x = anaL' coalg n0 where
>   coalg 0 = Nothing
>   coalg n = Just (x, n-1)

> rep = U.t "rep" (replicate 4 '*') "****"

----

> fibs :: [Integer]
> fibs = anaL (\(a, b) -> (a, (b, a + b)))
>             (0, 1)

> fib = U.t "fib" (fibs !! 7) 13

----

> linesBy :: (t -> Bool) -> [t] -> [[t]]
> linesBy p = anaL' c where
>   c []  = Nothing
>   c xs  = Just $ second (drop 1) $ break p xs

> lb = U.t "lb"
>      (linesBy (==',') "foo,bar,baz")
>      ["foo","bar","baz"]

----

~~~{.haskell}
break (==',') "foo,bar,baz"
=> ("foo",",bar,baz")


second (drop 1) ("foo",",bar,baz")
=> ("foo","bar,baz")
~~~


----

example: merging lists
----------------------

given 2 sorted, produce 1 sorted list

\small

> mergeLists :: Ord a => [a] -> [a] -> [a]
> mergeLists = curry $ anaL' c where

\iffalse

>   c :: Ord a => ([a], [a]) -> Maybe (a, ([a], [a]))

\fi

>   c (  [],   [])          = Nothing
>   c (  [], y:ys)          = Just (y, (  [],   ys))
>   c (x:xs,   [])          = Just (x, (  xs,   []))
>   c (x:xs, y:ys) | x <= y = Just (x, (  xs, y:ys))
>                  | x >  y = Just (y, (x:xs,   ys))

\iffalse

>   c (_:_ , _:_)           = matchAll "mergeLists"

\fi

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

> tsf = U.t "tsf"
>       (take 6 sFrom1)
>       [1,2,3,4,5,6]

------------------------------------------------------------------------------

\textbf{hylomorphism}
---------------------

composition of catamorphism and anamorphism

- corecursive codata production
- followed by recursive data consumption

> hyloL :: (a -> c -> c)       -- cata f
>       -> c                   -- cata zero
>       -> (b -> Maybe (a, b)) -- ana g
>       -> b                   -- ana seed
>       -> c                   -- result
> hyloL f c g = cataL f c . anaL' g

----

> fact :: Integer -> Integer
> fact n0 = hyloL' a 1 c n0 where
>   c 0 = Nothing
>   c n = Just (n, n - 1)
>   a   = (*)

> hf = U.t "hf" (fact 5) 120

----

\textbf{fusion/deforestation}

~~~{.haskell}
hyloL f c g b = cataL f c . anaL' g b
~~~

> hyloL' f a g b0 = h b0 where
>   h b = case g b of
>     Nothing       -> a
>     Just (a', b') -> f a' (h b')

------------------------------------------------------------------------------

\textbf{paramorphism}
---------------------

*para* meaning *beside* (or "parallel with")

extension of catamorphism

- given each element, and
- current cursor in iteration (e.g., current tail)

> paraL  :: (a -> [a] -> b -> b) -- f
>        -> b                    -- zero
>        -> [a]                  -- input
>        -> b                    -- output
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
- extension of anamorphism
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
>   c      []         = Nothing
>   c     [x]         = Just (x, Left     [])
>   c (x:y:xs) | x<=y = Just (x, Right (y:xs)) -- DONE
>              | x> y = Just (y, Left  (x:xs))

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
>       -> (a -> b -> c -> c) -- g depends on f result
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

- gives access to previously computed values
- moves bottom-up annotating stack with results

----

> data History a b
>   = End b | Step a b (History a b)
>     deriving (Eq, Read, Show)

> history :: (a -> History a b -> b)
>         -> b -> [a] -> History a b
> history f b = cataL (\a h -> Step a (f a h)h)
>                     (End b)

> valH (End    b)   = b
> valH (Step _ b _) = b

> histoL :: (a -> History a b -> b)
>        -> b -> [a] -> b
> histoL f b = valH . history f b

----

> thistory = U.t "thistory"
>   (history (\a _ -> show a) "" [1,2,3])
>   (Step 1 "1"
>           (Step 2 "2"
>                   (Step 3 "3"
>                           (End ""))))

----

> prevH (Step _ _ h) = h
> prevH z            = z

----

> fibHL :: Integer -> History Integer Integer
> fibHL n = history f 1 [3..n] where
>   f _ h = valH h + valH (prevH h)

> tfibHL = U.t "tfibHL"
>   (fibHL 8)
>   (Step 3 21
>     (Step 4 13
>       (Step 5 8
>         (Step 6 5
>           (Step 7 3
>             (Step 8 2
>               (End 1)))))))


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
>   coa xs = Just ( head (tail xs)
>                 , ( [head xs]
>                   , Just (tail (tail xs))
>                   )
>                 )

> exs1 = U.t "exs1"
>        (take 10 $ exchL sFrom1)
>        [2,1,4,3,6,5,8,7,10,9]

> exs2 = U.t "exs2"
>        (take  9 $ exchL sFrom1)
>        [2,1,4,3,6,5,8,7,10]

------------------------------------------------------------------------------

\begin{center}

\textbf{\Large{refactoring recursion\\
out of data}}

\end{center}

----

> data Tree1 a
>   = Leaf a
>   | Bin (Tree1 a) (Tree1 a)
>   deriving (Foldable)

> ext1 = Bin (Bin (Leaf "1") (Leaf "2"))
>            (Bin (Leaf "3") (Leaf "4"))

> et1 = U.t "et1"
>       (foldr (++) "" ext1)
>       "1234"

----

~~~{.haskell}
data LTreeF a r
  = LeafF a
  | BinF r r
  deriving (Functor)
~~~

> type Tree a    = Fix (LTreeF a)

~~~{.haskell}
leaf a  = Fix (LeafF a)
bin l r = Fix (BinF l r)
~~~

> ext = bin (bin (leaf "1") (leaf "2"))
>           (bin (leaf "3") (leaf "4"))

----

> cata :: Functor f => (f a -> a) -> Fix f -> a
> cata alg = alg . fmap (cata alg) . unFix

> sumT = cata alg where
>   alg (LeafF a)  = a
>   alg (BinF l r) = l ++ r

> et = U.t "et"
>      (sumT ext)
>      "1234"

------------------------------------------------------------------------------

\textbf{references}
-------------------

\small

Tim Williams' recursion schemes presentation

    - http://www.timphilipwilliams.com/slides.html
    - https://www.youtube.com/watch?v=Zw9KeP3OzpU

Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire

    - https://maartenfokkinga.github.io/utwente/mmf91m.pdf

These slides

    - https://github.com/haroldcarr/presentations/
      blob/master/2017-05-27-lambdaconf-recursion-schemes
      .pdf

\normalsize

\iffalse

> testRSL17 :: IO Counts
> testRSL17 =
>   runTestTT $ TestList $ c1 ++ cd ++ c2 ++ rep ++ fib ++ lb ++ ml ++ tsf ++ hf ++
>                          p1 ++ sl ++ iel ++ zpm ++ zpm' ++ thistory ++ tfibHL ++ exs1 ++ exs2 ++
>                          et1 ++ et

> matchAll :: String -> a
> matchAll msg = error (msg ++ " match all pattern")

\fi

