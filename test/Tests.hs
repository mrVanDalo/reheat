{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Tests
-- Copyright   :  GPL
-- License     :  GPL Nothing
--
-- Maintainer  :  Ingolf Wagner <palipalo9@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Tests where

import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)

import Task

-- Entry point for unit tests.
testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure

t1 = Comment "t1" []
t2 = Comment "t2" []
t3 = Comment "t3" []
t4 = Comment "t4" []
t5 = Comment "t5" []
t6 = Comment "t6" []

tasks1 = Tasks [t1,t2,t3,t4,t5,t6] []
tasks2 = Tasks [t1,t3,t2,t4,t5,t6] []
tasks3 = Tasks [t3,t2,t1,t4,t5,t6] []


prop_testSwap1 :: Int -> Int -> Bool
prop_testSwap1 a b = (swapTasks a b (swapTasks a b tasks1)) == tasks1

prop_testSwap2 :: Int -> Int -> Bool
prop_testSwap2 a b = (swapTasks a b tasks1) == (swapTasks b a tasks1)


prop_testSwap3 :: Bool
prop_testSwap3 = (swapTasks 1 2 tasks1) == tasks2

prop_testSwap4 :: Bool
prop_testSwap4 = (swapTasks 1 2 tasks2) == tasks1

prop_testSwap5 :: Bool
prop_testSwap5 = (swapTasks 0 2 tasks1) == tasks3
