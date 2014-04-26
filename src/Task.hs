-----------------------------------------------------------------------------
--
-- Module      :  Task
-- Copyright   :  GPL v3
-- License     :  AllRightsReserved
--
-- Maintainer  :  Ingolf Wagner <palipalo9@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | The Task interaction Module
--
-----------------------------------------------------------------------------
module Task where
{-
module Task (
    Task(..),
    Tasks,
    goInto,
    goOut,
    actualTasks,
    addTask,
    addTasks,
    removeTask,
    emptyTasks,
    renderToStrings,
    stringToTasks
) where
-}

import Control.Arrow
import Data.List

data Task = Comment { text :: String , children :: [Task] } deriving (Show, Eq,Read)
data Tasks = Tasks { actual :: [Task], history ::  [FTask] } deriving (Show, Read)
data FTask = FTask { focused :: Task, prev  :: [Task], next :: [Task] } deriving (Show,Read)


emptyTasks :: Tasks
emptyTasks = Tasks []  []

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
goOut (Tasks _ ((FTask a b c):xs))   =
    let actual = b ++ a:c
        rest   = case xs of
            []                   -> []
            ((FTask n w v):ks)   -> (FTask n { children = actual }  w v):ks
    in
        Tasks actual rest

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
            c = ys ++ (tail zs)
            rest = case (history tasks) of
                []                -> []
                (FTask p x  y):xs -> (FTask p { children = c }  x  y):xs
        in Tasks c rest

-- | create list of strings formated to write to file
renderToStrings :: Tasks -> [String]
renderToStrings = asStrings . asList

asStrings :: [Task] -> [String]
asStrings tasks = helper 0 tasks
    where
    ntimes n        = foldl (++) [] $ replicate n " "
    helper n []     = []
    helper n (x:xs) =
        let space    = ntimes n
            thisBlog = map (space ++) $ lines (text x)
            kinder   = helper (n + 1) (children x)
            rest     = helper n xs
        in
            thisBlog ++ kinder ++ ["\n"] ++ rest


-- | only needed for the goInto in the parsing
getNextActiveTask :: Tasks -> Maybe Task
getNextActiveTask (Tasks [] _)  = Nothing
getNextActiveTask (Tasks x _)   = Just . last $ x

-- | depth of the actual focused tasks 0 for top level
depth :: Tasks -> Int
depth (Tasks _  xs) = (length xs) - 1

-- | unrolls the tasks untill we have the toplevel
asList :: Tasks -> [Task]
asList = actual . unroll

-- | unrolls the tasks untill we have the toplevel
unroll :: Tasks -> Tasks
unroll (Tasks a []) = Tasks a []
unroll f            = unroll . goOut $ f
