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
paraL f b (a : as) = f a as (paraL  f b as)
paraL _ b []       = b

-- gives access to all previous values
histoL :: ([a] -> a) -> [a] -> a
histoL f = head . go where
    go [] = [f []]
    go xs = let subvalues = go xs in f subvalues : subvalues

-- TODO: zygo
-- TODO https://hackage.haskell.org/package/pointless-haskell-0.0.9/docs/src/Generics-Pointless-RecursionPatterns.html#Zygo
-- TODO http://dissertations.ub.rug.nl/faculties/science/1990/g.r.malcolm/
-- TODO above thesis: http://cgi.csc.liv.ac.uk/~grant/PS/thesis.pdf

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
