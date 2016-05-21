module Z where

import           Control.Arrow         ((&&&))
import           Data.Functor.Foldable (Fix (..))
import           RS
import           RSL

algZygo :: Functor f =>
    (f     b  -> b) ->
    (f (a, b) -> a) ->
     f (a, b)       ->
       (a, b)
algZygo f g = g &&& f . fmap snd

zygo :: Functor f =>
        (f b -> b) -> (f (a, b) -> a) -> Fix f -> a
zygo f g = fst . cata (Z.algZygo f g)

------------------------------------------------------------------------------

zygoL :: (a -> b -> b)      -> -- folding fun 1
         (a -> b -> c -> c) -> -- folding fun 2 : depends on result of 1st fold
         b -> c             -> -- zeroes for the two folds
         [a]                -> -- input list
         c                     -- result
zygoL f g b0 c0 = snd . cataL (\a (b, c) -> (f a b, g a b c)) (b0, c0)
