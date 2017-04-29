{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}

{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module TreeF where

import           Data.Functor.Foldable (Fix (..))
import           Test.HUnit            (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util       as U (t)

data T' a
  = E'
  | L' a
  | B' (T' a) a (T' a)
  deriving (Eq, Foldable, Functor, Show, Traversable)

data T a
  = L a
  | B (T a) (T a)
  deriving (Eq, Foldable, Functor, Show, Traversable)

ext1 = B (B (L "1") (L "2"))
         (B (L "3") (L "4"))

et1 = U.t "et1"
      (foldr (++) "" ext1)
      "1234"

data LTreeF a r
  = LeafF a
  | BinF r r
  deriving (Functor)

{-
instance Functor (LTreeF a) where
  fmap _ (Leaf a)    = Leaf a
  fmap f (Bin r1 r2) = Bin (f r1) (f r2)
-}

type Tree a    = Fix (LTreeF a)

leaf a  = Fix (LeafF a)
bin l r = Fix (BinF l r)

------------------------------------------------------------------------------

testTreeF :: IO Counts
testTreeF =
   runTestTT $ TestList {- $ -}  et1
