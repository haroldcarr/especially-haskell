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

-- gives access to all previous values
histoL ::     ([a]      -> a)      -> [a] -> a
histoL f = head . go where
    go [] = [f []]
    go xs = let histvals = go xs in f histvals : histvals

-- TODO: zygo
-- TODO https://hackage.haskell.org/package/pointless-haskell-0.0.9/docs/src/Generics-Pointless-RecursionPatterns.html#Zygo
-- TODO http://dissertations.ub.rug.nl/faculties/science/1990/g.r.malcolm/
-- TODO above thesis: http://cgi.csc.liv.ac.uk/~grant/PS/thesis.pdf
-- TODO Refining Inductive Types http://arxiv.org/pdf/1205.2492.pdf
-- TODO Has definition, but don't understand yet: http://www.iis.sinica.edu.tw/~scm/pub/mds.pdf

-- semi-mutual iteration where two functions are defined simultaneously
-- one being the function of interest
-- the other an auxiliary function

-- see comments above paraL
-- zygo ∷ (f (a , b) → a) → (f b → b)       → μf → a
-- I do not think this is right, but getting closer.
zygoL :: (a -> [a] -> b -> b) -> (b -> b) -> b -> [a] -> b
zygoL f g b (a : as) = f a as (zygoL f g b as)
zygoL _ g b []       = g b

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

hyloL :: (a -> c -> c) -> c -> (b -> Maybe (a, b)) -> b -> c
hyloL f z g = cataL f z . anaL' g

-- fusion/deforestation
hyloL':: (t -> t1 -> t1) -> t1 -> (t1 -> Maybe (t, t1)) -> t1
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

zlide :: Show a => Int -> [a] -> [String]
zlide n = zygoL alg1 alg2 [] where
    alg1 _ [] b                     = b
    alg1 a as b | length (a:as) < n = b
                | otherwise         = show (take n (a:as)) : b
    -- alg2 :: (Show b) => b -> String
    alg2      b                     = [show b]

z1 :: [Test]
z1 = U.t "z1"
    (zlide 3 [1..6::Int])
    ["[1,2,3]","[2,3,4]","[3,4,5]","[4,5,6]","[]"]

test :: IO Counts
test =
    runTestTT $ TestList $ c1 ++ c2 ++ p1 ++ p2 ++ z1
