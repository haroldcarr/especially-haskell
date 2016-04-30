module Futu where

{-
http://stackoverflow.com/a/36923571/814846

ListF a b = Base [a] = ConsF a b | NilF

futu :: Unfoldable t => (a -> Base  t  (Free (Base  t)  a)) -> a ->  t

ignore Unfoldable constraint
substitute [b] for t

                        (a -> Base [b] (Free (Base [b]) a)) -> a -> [b]
                        (a -> ListF b  (Free (ListF b)  a)) -> a -> [b]
                                       (Free (ListF b)  a) is a list, possibly with a hole at end
means it is isomorphic to ([b], Maybe a), giving:
                        (a -> ListF b       ([b], Maybe a)) -> a -> [b]
then, eliminate last ListF, since ListF a b is isomorphic to Maybe (a, b):


-}
-- | takes a seed value and a function that may produce at least one result
--   and possibly a new seed value if it produced a result
futuL ::                (a -> Maybe (b,     ([b], Maybe a)))-> a -> [b]
futuL f x =
  case f x of
    Nothing -> []
    Just (y, (ys, mz)) -> y : (ys ++ fz)
      where fz = case mz of
                   Nothing -> []
                   Just z -> futuL f z
{-
Equivalent to

notFutuL :: (a -> ([b], Maybe a)) -> a -> [b]
notFutuL f x = case f x of
  (ys, mx) -> ys ++ case mx of
    Nothing -> []
    Just x' -> notFutuL x'

except `futu` guarantees productivity
- if `f` returns, then not stuck waiting for next element
-}
