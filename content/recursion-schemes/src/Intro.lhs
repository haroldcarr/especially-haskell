> module Intro where

\textbf{introduction}
---------------------

- Recursion is a pattern
- There are different patterns of recursion
- "factoring" recursion : benefits
    - communicate/reason about programs
    - code/idea reuse
    - use "proven loops" --- less bugs
    - use a catalogue of theorems to optimise or prove properties
    - identify/exploit parallelism

\textbf{overview}
-----------------

- explicit recursive functions
- factor recursion out of functions with `fold`
- explicit recursive data
- factor recursion out of data
    - `Foldable`, `Traversable`, `Fix`
- use "library" functions to recursivly operate on "fixed" data
    - generic "folds" (aka "catamorphism")
    - generic "unfolds" (aka "anamorphism")
    - generic unfolds followed by folds (aka "hylomorphism")
- conclusion
- other recursion schemes (time permitting)
- foundations and advanced usage (in the hall)

- catamorphisms, anamorphisms hylomorphisms
    - folds, unfolds, and refolds
    - fundamental
    - they can express all recursive computation
- other recursion schemes : based on the above
    - offer more structure
- enable reliable, efficient, parallel programs

Recursion   Corecursion   General
----------  ------------  ---------
cata        ana           hylo
para        apo
histo       futu
zygo

\setlength{\tabcolsep}{8pt}
\renewcommand{\arraystretch}{1.5}

\begin{tabular}{ l l p{6cm} }
\textbf{recursion} / \textbf{data}  &      & \textbf{corecursion} / \textbf{codata} \\
\hline
cata            &      & ana \\
                & hylo &  \\
para (cata++)   &      & apo (ana++) \\
histo           &      & futu \\
zygo (para gen) &      &  \\
\end{tabular}


\textbf{other recursion schemes}
--------------------------------

\begin{tabular}{ l l p{6cm} }
`para`  &    paramorphism  & folds with access to input arg corresponding to most recent state of computation \\
`apo`   &    apomorphism   & TODO unfold that can exit early \\
`zygo`  &    zygomorphism  & TODO generalized paramorphism : asymmetric mutual recursion \\
`histo` &    histomorphism & TODO course-of-value recursion : can use previously computed values \\
`futu`  &    futumorphism  & TODO course-of-value coiteration : produce one or more levels \\
\end{tabular}
