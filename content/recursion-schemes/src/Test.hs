{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Test where

import           Ana
import           Apo
import           Cata
import           ExprF
import           Foldable
import           Futu
import           Histo
import           Hylo
import           NatF
import           Para
import           RS
import           RSL17
import           Test.HUnit (Counts)
import           Zygo

test :: IO Counts
test  = do
  testAna
  testApo
  testCata
  testExprF
  testFoldable
  testFutu
  testHisto
  testHylo
  testNatF
  testPara
  testRS
  testRSL17
  testZygo
