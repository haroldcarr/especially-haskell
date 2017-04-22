> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
>
> module Explicit where

Explicit Recursion
==================

note the pattern

> sumE    []  = 0
> sumE (x:xs) = x +  sumE xs

> andE    []  = True
> andE (x:xs) = x && andE xs

same recursive structure, except

- `0` or `True`
    - for base case (i.e., empty list)
- `+` or `&&`
    - for operator in inductive case

factor recursion out of functions with `fold`
============================================

\iffalse

> sumF :: (Foldable t, Num b) => t b    -> b

\fi

> sumF  = foldr (+)  0

\iffalse

> andF :: Foldable t          => t Bool -> Bool

\fi

> andF  = foldr (&&) True

~~~{.haskell}
     sumF                  andF
      +                     &&
     / \                   /  \
    1   +               True   &&
       / \                    /  \
      2   +               False   &&
         / \                     /  \
        3   0                 True  True
~~~

----

another example: `length`

> lengthE []     = 0
> lengthE (_:xs) = 1 + lengthE xs

as a fold

\iffalse

> lengthF :: (Foldable t, Num b) => t a -> b

\fi

> lengthF        = foldr  (\_ n -> 1 + n)  0

~~~{.haskell}
lengthFL       = foldl' (const . P.succ) 0

 sumF                  andF                  lengthF
  +                     &&                      1+
 / \                   /  \                    /  \
1   +               True   &&                 _    1+
   / \                    /  \                    /  \
  2   +               False   &&                 _    1+
     / \                     /  \                    /  \
    3   0                 True  True                _    0
~~~

----

to understand how this works, look at def of `foldr` :

~~~{.haskell}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)
~~~

~~~{.haskell}
 sumF                  andF                  lengthF
  +                     &&                      1+
 / \                   /  \                    /  \
1   +               True   &&                 _    1+
   / \                    /  \                    /  \
  2   +               False   &&                 _    1+
     / \                     /  \                    /  \
    3   0                 True  True                _    0
~~~
