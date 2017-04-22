{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Test where

import           Ana
import           Cata
import           Foldable
import           Hylo
import           NatF
import           Para
import           RS
import           RSL
import           RSL17
import           Test.HUnit (Counts)

test :: IO Counts
test  = do
  testAna
  testCata
  testFoldable
  testHylo
  testNatF
  testPara
  testRS
  testRSL
  testRSL17



