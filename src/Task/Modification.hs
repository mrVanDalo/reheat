-----------------------------------------------------------------------------
--
-- Module      :  Task.Modification
-- Copyright   :  GPL v3
-- License     :  AllRightsReserved
--
-- Maintainer  :  Ingolf Wagner <palipalo9@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | This package contains all the manipulation you can do to the
-- Task objects.
--
-----------------------------------------------------------------------------

module Task.Modification where


import Control.Arrow
import Data.List
import Task.Data

-- | bread crumbs
breadCrumbs :: Tasks -> [Task]
breadCrumbs (Tasks _ ftasks) = map focused ftasks

-- | swap tasks on the actual path of the Tasks object
swapTasks :: Int -> Int -> Tasks -> Tasks
swapTasks a b tasks
    | a < 0                      = tasks
    | b < 0                      = tasks
    | a == b                     = tasks
    | a >= length (actual tasks) = tasks
    | b >= length (actual tasks) = tasks
    | otherwise                  =
        let t = actual tasks
            x = t !! a
            y = t !! b
        in  Tasks (map (swapper x y)  t) (history tasks)
        where
            swapper x y l
                | l == x = y
                | l == y = x
                | otherwise = l


-- | go into the children of some tasks
goInto :: Task-> Tasks-> Tasks
goInto task (Tasks ys xs) =
    let pos  = task `elemIndex` ys
        elem = case pos of
            Nothing -> FTask task ys  []
            Just n  -> FTask task (take n ys) (drop (n + 1) ys)
    in Tasks (children task)  (elem:xs)

-- | go out of actual task list and go one level up
goOut :: Tasks -> Tasks
goOut (Tasks a [])                   = Tasks a []
goOut (Tasks childs ((FTask a b c):xs))   =
    let actual = b ++ (a { children = childs} ):c
        rest   = case xs of
            []                   -> []
            ((FTask n w v):ks)   -> (FTask n w v):ks
    in  Tasks actual rest

-- | Add Task to Tasks
addTask :: Task -> Tasks -> Tasks
addTask t (Tasks a [])                      = Tasks (t:a) []
addTask t (Tasks a ((FTask parent b c):bs)) =
    Tasks (t:a) ((FTask parent { children = t:(children parent)} b c):bs)

-- | Add Tasks to Tasks
addTasks :: [Task] -> Tasks -> Tasks
addTasks t (Tasks a [])                      = Tasks (t++a) []
addTasks t (Tasks a ((FTask parent b c):bs)) =
    Tasks (t ++ a) ((FTask parent { children = t ++ (children parent)} b c):bs)



-- | removes Task from Tasks
removeTask :: Int -> Tasks -> Tasks
removeTask index tasks
    | index < 0                       = tasks
    | index > (length $ actual tasks) = tasks
    | otherwise                       =
        let (ys,zs) = splitAt index (actual tasks)
            c       = ys ++ (tail zs)
            rest    = case (history tasks) of
                []                -> []
                (FTask p x  y):xs -> (FTask p { children = c }  x  y):xs
        in Tasks c rest

-- | depth of the history
depth :: Tasks -> Int
depth = length . history

-- | unrolls the tasks untill we have the toplevel
asList :: Tasks -> [Task]
asList = actual . unroll

-- | unrolls the tasks untill we have the toplevel
unroll :: Tasks -> Tasks
unroll (Tasks a []) = Tasks a []
unroll f            = unroll . goOut $ f
