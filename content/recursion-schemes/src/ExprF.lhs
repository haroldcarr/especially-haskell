> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
>
> {-# LANGUAGE DeriveFoldable    #-}
> {-# LANGUAGE DeriveFunctor     #-}
> {-# LANGUAGE DeriveTraversable #-}
>
> module ExprF where
>
> import           Cata                  (cata)
> import           Data.Bool.Extras      (bool)
> import           Data.Functor.Foldable (Fix (..))
> import           Data.Map              as M (Map, fromList, lookup)
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t)

> data ExprF r = Const Int
>              | Var   Id
>              | Add   r r
>              | Mul   r r
>              | IfNeg r r r
>                deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

> type Id = String

> type Expr = Fix ExprF

*pattern functor* `ExprF` represents structure of `Expr`

isomorphism between data-type and its pattern functor type handled by `Fix` and `unFix`

------------------------------------------------------------------------------
usage

example: evaluator with global environment
------------------------------------------

> type Env = Map Id Int

> eval :: Env -> Expr -> Maybe Int
> eval env = cata (evalAlg env)

> evalAlg :: Env -> ExprF (Maybe Int) -> Maybe Int
> evalAlg env = alg where
>   alg (Const c)     = pure c
>   alg (Var i)       = M.lookup i env
>   alg (Add x y)     = (+) <$> x <*> y
>   alg (Mul x y)     = (*) <$> x <*> y
>   alg (IfNeg t x y) = t >>= bool x y . (<0)

----

> e1 :: Fix ExprF
> e1 = Fix (Mul
>            (Fix (IfNeg
>                   (Fix (Mul (Fix (Const 1))
>                             (Fix (Var "a"))))
>                   (Fix (Add (Fix (Var "b"))
>                             (Fix (Const 0))))
>                   (Fix (Add (Fix (Var "b"))
>                             (Fix (Const 2))))))
>                 (Fix (Const 3)))

----

an example expression
---------------------

\vspace{0.2in}
\centerline{\hbox{%
\begin{tikzpicture}[]
\Tree [.Mul [.IfNeg [.Mul [.Const 1 ] [.Var a ] ] [.Add [.Var b ] [.Const 0 ] ] [.Add [.Var b ] [.Const 2 ] ] ] [.Const 3 ] ]
\end{tikzpicture}
}}

> testEnv :: Env
> testEnv = M.fromList [("a",1),("b",3)]

> ee1 = U.t "ee1"
>       (eval testEnv e1)
>       (Just 9)

- expression `e2` is a function of variables `a` and `b`

> e2 :: Fix ExprF
> e2 = Fix (IfNeg (Fix (Var "b")) e1 (Fix (Const 4)))

------------------------------------------------------------------------------

> testExprF :: IO Counts
> testExprF  =
>     runTestTT $ TestList {- $ -} ee1


