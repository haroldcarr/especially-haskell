module RSL where

-- TODO http://conal.net/talks/folds-and-unfolds.pdf
-- TODO http://comonad.com/reader/2009/recursion-schemes/
-- TODO https://hackage.haskell.org/package/pointless-haskell-0.0.9/docs/Generics-Pointless-Examples-Examples.html
-- TODO https://hackage.haskell.org/package/pointless-haskell-0.0.9/docs/Generics-Pointless-RecursionPatterns.html
-- TODO http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/urs.pdf
-- TODO http://cs.ioc.ee/~tarmo/papers/nwpt97-peas.pdf
-- TODO http://www.amazon.com/Algebraic-Coalgebraic-Methods-Mathematics-Construction/dp/3540436138
-- TODO http://www.staff.science.uu.nl/~jeuri101/afp/afp4/
-- TODO http://www.mii.lt/informatica/pdf/INFO141.pdf
-- TODO http://comonad.com/reader/2008/time-for-chronomorphisms/

import           Test.HUnit      (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util as U (t, tt)

-- http://stackoverflow.com/questions/36851766/list-only-version-of-zygomorphism-and-futumorphism-recursion-schemes

------------------------------------------------------------------------------
-- Recursion : cata  para  histo  zygo
{-
inductive recursion : each recursive step consumes one or more constructors
- ensures terminates (if given finite input)
-}
cataL :: (a ->        b -> b) -> b -> [a] -> b
cataL f b (a : as) = f a    (cataL f b as)
cataL _ b []       = b

paraL :: (a -> [a] -> b -> b) -> b -> [a] -> b
paraL f b (a : as) = f a as (paraL f b as)
paraL _ b []       = b

-- http://b-studios.de/blog/2016/02/21/the-hitchhikers-guide-to-morphisms/
-- defines:
-- para ∷    (f (μf , a)-> a)      -> μf  -> a
-- which might be (for non-empty lists):
-- para ∷     ([a] -> b -> b)      -> [a] -> b
paraL' :: (     [a] -> b -> b) -> b -> [a] -> b
paraL' f b as@(_:xs) = f as (paraL' f b xs)
paraL' _ b []        = b

-- http://stackoverflow.com/a/24892711/814846
-- gives access to all previous values
histoL ::     ([a]      -> a)      -> [a] -> a
histoL f = head . go where
    go [] = [f []]
    go as = let histvals = go as in f histvals : histvals

zygoL :: (a -> b -> b)      -> -- folding fun 1
         (a -> b -> c -> c) -> -- folding fun 2 : depends on result of 1st fold
         b -> c             -> -- zeroes for the two folds
         [a]                -> -- input list
         c                     -- result
zygoL f g b0 c0 = snd . cataL (\a (b, c) -> (f a b, g a b c)) (b0, c0)

------------------------------------------------------------------------------
-- Corecursion : ana  apo  futu

{-
co-inductive co-recursion : each recursive step guarded by a constructor
- although result can be infinite, each step (a constructor) is produced in finite time (i.e., makes progress)
-}
anaL  :: (b ->       (a, b))               -> b -> [a]
anaL  f b = let (a, b') = f b in a : anaL f b'

anaL' :: (b -> Maybe (a, b))               -> b -> [a]
anaL' f b = case f b of
    Just (a, b') -> a : anaL' f b'
    Nothing      -> []

-- apo
-- TODO http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.35.7317&rep=rep1&type=pdf
-- https://books.google.com/books?id=xycOgASxEWQC&pg=PA155&lpg=PA155&dq=apomorphism&source=bl&ots=WYGhIdOAgy&sig=DpbWd5S4dgUb89PllF4kMOWEu14&hl=en&sa=X&ved=0ahUKEwjL3_jYsKjMAhUks4MKHc2nDBo4FBDoAQglMAM#v=onepage&q=apomorphism&f=false
-- primitive corecursion (dual of para)
-- on finite inputs, same as `anaL` but with results of `h` applied to final element appended
apoL  :: (b -> Maybe (a, b)) -> (b -> [a]) -> b -> [a]
apoL f h b = case f b of
    Just (a, b') -> a : apoL f h b'
    Nothing      -> h b

-- TODO futu
{-
histo : access to previously-computed values
futu  : access to values that recursion will compute in future
http://jtobin.ca/time-traveling-recursion
-}

------------------------------------------------------------------------------
-- General: hylo

-- Recursion Patterns as Hylomorphisms
-- http://www4.di.uminho.pt/~mac/Publications/DI-PURe-031101.pdf

hyloL :: (a -> c -> c) -> c -> (b -> Maybe (a, b)) -> b -> c
hyloL f z g = cataL f z . anaL' g

-- fusion/deforestation
hyloL':: (a -> c -> c) -> c -> (c -> Maybe (a, c)) -> c
hyloL' f z g = case g z of
    Nothing     -> z
    Just (x,z') -> f x (hyloL' f z' g)

------------------------------------------------------------------------------
-- test

c1 :: [Test]
c1 = U.t "c1"
    (cataL ((:) . (+1)) [] [1,2,3::Int])
    [2,3,4]

c2 :: [Test]
c2 = U.t "c2"
    (cataL (+) 0 [1,2,3::Int])
    6

tails :: [a] -> [[a]]
tails = paraL (\a as b -> (a:as):b) []

p1 :: [Test]
p1 = U.t "p1"
    (tails [1,2,3,4::Int])
    [[1,2,3,4],[2,3,4],[3,4],[4]]

slide :: Int -> [a] -> [[a]]
slide n = paraL alg [] where
    alg _ [] b                     = b
    alg a as b | length (a:as) < n = b
               | otherwise         = take n (a:as) : b

slide' :: Int -> [a] -> [[a]]
slide' n = paraL' alg [] where
    alg [] b                 = b
    alg as b | length as < n = b
             | otherwise     = take n as : b

p2 :: [Test]
p2 = U.tt "p2"
    [ slide  3 [1..6::Int]
    , slide' 3 [1..6::Int]
    ]
    [[1,2,3],[2,3,4],[3,4,5],[4,5,6]]

test :: IO Counts
test =
    runTestTT $ TestList $ c1 ++ c2 ++ p1 ++ p2
