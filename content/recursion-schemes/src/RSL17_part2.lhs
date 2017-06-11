\iffalse

> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults      #-}
>
> module RSL17_part2 where
>
> import           Test.HUnit                    (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util               as U (t, tt)
>

\fi

\begin{center}
\textbf{\LARGE{Refactoring Recursion}}
\textbf{\LARGE{part 2}}
\Large{Harold Carr}
\end{center}

\normalsize

----

\textbf{introduction}
---------------------

\iffalse

> testRSL17_part2 :: IO Counts
> testRSL17_part2 =
>   runTestTT $ TestList $ [] ++ []

\fi

