{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-
Created       : 2014 Feb 26 (Wed) 18:54:30 by Harold Carr.
Last Modified : 2015 Oct 28 (Wed) 18:02:15 by Harold Carr.
-}

module WriteRunDot where

import           Control.Monad   (forM_)
import           Data.GraphViz
import           System.FilePath

doDots :: PrintDotRepr dg n => FilePath -> [(FilePath, dg n)] -> IO ()
doDots dir cases = forM_ cases (createImage dir)

createImage :: PrintDotRepr dg n => FilePath -> (FilePath, dg n) -> IO FilePath
createImage dir (n, g) = createImageInDir dir n Png g

createImageInDir :: PrintDotRepr dg n => FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir d n o g = Data.GraphViz.addExtension (runGraphvizCommand Dot g) o (combine d n)

-- End of file.
