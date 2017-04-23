> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
>
> module Zygo where
>
> import           Cata                  (cata, cataL)
> import           Control.Arrow         ((&&&))
> import           Data.Bool.Extras      (bool)
> import           Data.Foldable         (fold)
> import           Data.Functor.Foldable (Fix(..))
> import           Data.Map              as M (fromList)
> import           Data.Maybe            (isJust)
> import           Data.Monoid           (Sum (..), getSum, (<>))
> import           ExprF
> import           Para                  (paraL)
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t)

------------------------------------------------------------------------------
definition

Zygomorphism
============

- asymmetric mutual iteration
    - both a data consumer and an auxiliary function are defined
- a generalisation of paramorphisms

> zygoL :: (a -> b -> b)      -> -- folding fun 1
>          (a -> b -> c -> c) -> -- folding fun 2 : depends on result of 1st fold
>          b -> c             -> -- zeroes for the two folds
>          [a]                -> -- input list
>          c                     -- result
> zygoL f g b0 c0 = snd . cataL (\a (b, c) -> (f a b, g a b c)) (b0, c0)

Zygomorphism pattern:
- fold that depends on result of another fold
- fuse into one traversal

On each iteration of the fold
- f sees its  answer  from previous iteration
- g sees both answers from previous iteration

> algZygo :: Functor f =>
>     (f     b  -> b) ->
>     (f (a, b) -> a) ->
>      f (a, b)       ->
>        (a, b)
> algZygo f g = g &&& f . fmap snd

> zygo :: Functor f =>
>         (f b -> b) -> (f (a, b) -> a) -> Fix f -> a
> zygo f g = fst . cata (algZygo f g)

------------------------------------------------------------------------------
usage

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

> di = U.t "di"
>      (disconts (M.fromList [("b",-1)]) e2)
>      1

==============================================================================
TODO

 https://hackage.haskell.org/package/pointless-haskell-0.0.9/docs/src/Generics-Pointless-RecursionPatterns.html#Zygo
 http://dissertations.ub.rug.nl/faculties/science/1990/g.r.malcolm/
 above thesis: http://cgi.csc.liv.ac.uk/~grant/PS/thesis.pdf
 Refining Inductive Types http://arxiv.org/pdf/1205.2492.pdf
 Has definition, but don't understand yet: http://www.iis.sinica.edu.tw/~scm/pub/mds.pdf

zygo : allows a directed dependency between two parallel folds.

http://stackoverflow.com/a/36911924/814846
Zygomorphism
- folds built from two semi-mutually-recursive functions.

E.G.:

> -- | intersperses + and - alternately through a list of numbers
> --   plusMinus [v,w,x,y,z] = v - (w + (x - (y + z)))
> --   primitive recursion version:
> plusMinusPR :: [Int] -> Int
> plusMinusPR [] = 0
> plusMinusPR (x:xs) =
>   if (even . length) xs then
>     x - plusMinusPR xs
>   else
>     x + plusMinusPR xs

> lengthEven :: [a] -> Bool
> lengthEven = even . length

plusMinusPR : not compositional : must inspect length of whole list to + or -

since paramorphism models primitive recursion, rewrite:

> plusMinusP :: [Int] -> Int
> plusMinusP = paraL (\x xs acc -> pm (lengthEven xs) x acc) 0

> pm :: Bool -> Int -> Int -> Int
> pm b x acc = if b then x - acc else x + acc

inefficient
- lengthEven traverses whole list at each iteration of paramorphism
- O(n^2)

Note : lengthEven and para can be expressed as catamorphisms

> lengthEvenC :: [a] -> Bool
> lengthEvenC = cataL (\_ p -> not p) True

> paraC :: (a -> [a] -> b -> b) -> b -> [a] -> b
> paraC f z = snd . cataL (\x (xs, acc) -> (x:xs, f x xs acc)) ([], z)

Suggests fusing the two operations:

> plusMinusC :: [Int] -> Int
> plusMinusC =
>   snd . cataL (\x (isEven, acc) -> (not isEven, pm isEven x acc))
>   (True, 0)

plusMinus as zygomorphism
- first  folding fun : determines even/odd
- second folding fun : calculates total

> plusMinusZ :: [Int] -> Int
> plusMinusZ = zygoL (\_ p -> not p)
>                    (\x isEven acc -> pm isEven x acc)
>                    True
>                    0

higher order function (zygo) consumes list - O(n)
- added logic to aggregate results

------------------------------------------------------------------------------

Generalising from lists to the fixpoint of an arbitrary functor:

newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para f = snd . cata (\x -> (Fix (fmap fst x), f x))

zygo :: Functor f => (f b -> b) -> (f (b, a) -> a) -> Fix f -> a
zygo f g = snd . cata (\x -> (f $ fmap fst x, g x))
Tough exercise: prove the fusion law para (g . fmap (mapFst (cata f))) = zygo f g, where mapFst f (x, y) = (f x, y).

You can recover the list version from the generalised version.

data ListF a r = Nil_ | Cons_ a r deriving Functor
type List a = Fix (ListF a)

zygoL' :: (a -> b -> b) -> (a -> b -> c -> c) -> b -> c -> List a -> c
zygoL' f g z e = zygo k l
    where k Nil_ = z
          k (Cons_ x y) = f x y
          l Nil_ = e
          l (Cons_ x (y, z)) = g x y z

pm4 = zygoL' (\_ p -> not p) (\x isEven total -> if isEven
                                                 then x - total
                                                 else x + total) True 0

------------------------------------------------------------------------------

> testZygo :: IO Counts
> testZygo  =
>     runTestTT $ TestList {- $ -} di
