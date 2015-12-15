-- org* setup
{-# LANGUAGE DeriveFunctor #-}

module PRS where

import           Data.Functor.Foldable (Fix(..), ana, cata, hylo, para)
import           Data.List.Ordered     (merge)
import           Prelude               hiding (Foldable, succ)
-- org* Natural
data Natural = Zero
             | Succ Natural
-- org* NatF
data NatF r = ZeroF
            | SuccF r
            deriving (Show, Functor) -- must be a functor
-- org* ListF_TreeF
data ListF a r = NilF
               | ConsF a r
               deriving (Show, Functor)

data TreeF a r = EmptyF
               | LeafF a
               | NodeF r r
               deriving (Show, Functor)
-- org* Fix_NatF
type Nat    = Fix NatF
type List a = Fix (ListF a)
type Tree a = Fix (TreeF a)
-- org* smart_constructors
zero :: Nat
zero  = Fix ZeroF

succ :: Nat -> Nat
succ  = Fix . SuccF

nil :: List a
nil  = Fix NilF

cons     :: a -> List a -> List a
cons x xs = Fix (ConsF x xs)
-- org* natsum
natsum :: Nat -> Int
natsum = cata alg where
    alg  ZeroF    = 0
    alg (SuccF n) = n + 1
-- org* filterL
filterL :: (a -> Bool) -> List a -> List a
filterL p = cata alg where
    alg  NilF                    = nil
    alg (ConsF x xs) | p x       = cons x xs
                     | otherwise = xs
-- org* nat
nat :: Int -> Nat
nat = ana coalg where
    coalg n | n <= 0    = ZeroF
            | otherwise = SuccF (n - 1)
-- org* natfac
natfac :: Nat -> Int
natfac = para alg where
    alg  ZeroF         = 1
    alg (SuccF (n, f)) = natsum (succ n) * f
-- org* natpred
natpred :: Nat -> Nat
natpred = para alg where
    alg  ZeroF         = zero
    alg (SuccF (n, _)) = n
-- org* tailL
tailL :: List a -> List a
tailL = para alg where
    alg  NilF            = nil
    alg (ConsF _ (l, _)) = l
-- org* mergeSort
mergeSort :: Ord a => [a] -> [a]
mergeSort = hylo alg coalg where
    alg EmptyF      = []
    alg (LeafF c)   = [c]
    alg (NodeF l r) = merge l r
    coalg []        = EmptyF
    coalg [x]       = LeafF x
    coalg xs        = NodeF l r where
       (l, r)       = splitAt (length xs `div` 2) xs
