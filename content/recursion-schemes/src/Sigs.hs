modules Sigs where

cataL :: (a ->        b -> b) -> b -> [a] -> b
paraL :: (a -> [a] -> b -> b) -> b -> [a] -> b
paraL':: (     [a] -> b -> b) -> b -> [a] -> b
histoL::      ([a]      -> a)      -> [a] -> a
zygoL :: (a -> b -> b)      -> -- folding fun 1
         (a -> b -> c -> c) -> -- folding fun 2 : depends on result of 1st fold
         b -> c             -> -- zeroes for the two folds
         [a]                -> -- input list
         c                     -- result
------------------------------------------------------------------------------
anaL  :: (b ->       (a, b))               -> b -> [a]
anaL' :: (b -> Maybe (a, b))               -> b -> [a]
apoL  :: (b -> Maybe (a, b)) -> (b -> [a]) -> b -> [a]
------------------------------------------------------------------------------
hyloL :: (a ->  c ->  c) ->  c -> (b  -> Maybe (a, b)) - >  b -> c
hyloL':: (t -> t1 -> t1) -> t1 -> (t1 -> Maybe (t, t1)) -> t1
