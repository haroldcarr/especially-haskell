> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> {-# OPTIONS_GHC -fno-warn-orphans            #-}
>
> {-# LANGUAGE DeriveFunctor         #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
>
> module NatF where
>
> import           Data.Functor.Classes  (Eq1(..), Show1(..), showsUnaryWith)
> import           Data.Functor.Foldable (Fix(..))
> import           Fixpoint
> import           Prelude               hiding (succ)
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t)

~~~{.haskell}
data Natural = Zero | Succ Natural
~~~

factor out recursion by defining base structure

- with parameterized type at recursive points

> -- "pattern functor" for `Natural` (must be a functor)
> data NatF r = ZeroF | SuccF r
>             deriving (Eq, Functor, Show)

> instance Fixpoint NatF Integer where
>   inF ZeroF          = 0
>   inF (SuccF n)      = n + 1
>   outF n | n > 0     = SuccF (n - 1)
>          | otherwise = ZeroF

> instance Eq1 NatF where
>   liftEq _   ZeroF     ZeroF    = True
>   liftEq _   ZeroF    (SuccF _) = False
>   liftEq _  (SuccF _)  ZeroF    = False
>   liftEq eq (SuccF l) (SuccF r) = eq l r

> instance Show1 NatF where
>   liftShowsPrec _  _ _  ZeroF    = showString "ZeroF"
>   liftShowsPrec sp _ d (SuccF n) = showsUnaryWith sp "SuccF" d n

> infz = U.t "infz" (inF ZeroF     :: Integer) 0
> infs = U.t "infs" (inF (SuccF 0) :: Integer) 1
> otfz = U.t "otfz" (outF 0 :: NatF Integer) ZeroF
> otfs = U.t "otfs" (outF 1 :: NatF Integer) (SuccF 0)

add recursion to pattern functors via recursive `Fix` wrapper

> type Nat    = Fix NatF

------------------------------------------------------------------------------

> ifi = U.t "ifi"
>     (inF       (SuccF (Fix ZeroF)))
>           (Fix (SuccF (Fix ZeroF)))

> ofi = U.t "ofi"
>     (outF (Fix (SuccF (Fix ZeroF))))
>                (SuccF (Fix ZeroF))

----

smart constructors

> zero  :: Nat
> zero  = Fix ZeroF

> succ  :: Nat -> Nat
> succ  = Fix . SuccF

> sc = U.t "sc"
>      (succ       (succ       (succ        zero     )))
>      (Fix (SuccF (Fix (SuccF (Fix (SuccF (Fix ZeroF)))))))

------------------------------------------------------------------------------

> testNatF :: IO Counts
> testNatF  =
>     runTestTT $ TestList $ infz ++ infs ++ otfz ++ otfs ++ ifi ++ ofi ++ sc
