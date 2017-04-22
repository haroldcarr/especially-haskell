{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}

module TreeF where

data Tree a = E
            | L a
            | B (Tree a) a (Tree a)
            deriving (Eq, Foldable, Functor, Show)

data LTreeF a r = Leaf a | Bin r r

instance Functor (LTreeF a) where
  fmap _ (Leaf a)    = Leaf a
  fmap f (Bin r1 r2) = Bin (f r1) (f r2)

