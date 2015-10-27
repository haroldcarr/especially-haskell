{-# LANGUAGE OverloadedStrings #-}

import           Data.List          as L (isPrefixOf)
import           Prelude            as P
import           System.Environment (getArgs)

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith f inFile outFile = do
    i <- readFile inFile
    writeFile outFile (f i)

orgIt :: String -> String
orgIt = P.unlines . go False . P.lines
  where
    beginSrc                 = "#+BEGIN_SRC haskell"
    endSrc                   = "#+END_SRC"
    go _ []                  = [endSrc]
    go trailerNeededP (l:ls) =
        let o = drop 6 l : beginSrc : go True ls
        in if "-- org*" `L.isPrefixOf` l
           then if trailerNeededP then endSrc : o else o
           else l : go trailerNeededP ls

main :: IO ()
main = do
    args <- getArgs
    case args of
        [i,o] -> interactWith orgIt i o
        _     -> putStrLn "usage: infra inFilename outFilename"
