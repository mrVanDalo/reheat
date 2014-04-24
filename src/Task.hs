-----------------------------------------------------------------------------
--
-- Module      :  Task
-- Copyright   :  Ingolf Wagner
-- License     :  GPL v3
--
-- Maintainer  :  Ingolf Wagner
-- Stability   :  stable
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
    lastTasks,
    addTask,
    addTasks,
    removeTask,
    emptyTasks,
    (-:),
    renderToStrings,
    stringToTasks
) where
-}

import Control.Arrow
import Data.List

data Task = Comment { text :: String , children :: [Task] } deriving (Show, Eq,Read)

data Tasks = Tasks { actual :: [Task], history ::  [FTask] } deriving (Show, Read)

type FTask = (Maybe Task, [Task], [Task])

emptyTasks :: Tasks
emptyTasks = Tasks []  [(Nothing, [],[])]

(-:) :: (a -> b) -> (b -> c) -> (a -> c)
g -: f =  f . g


-- | go into the children of some tasks
goInto :: Task-> Tasks-> Tasks
goInto task (Tasks ys xs) =
    let pos  = task `elemIndex` ys
        elem = case pos of
            Nothing -> (Just task, ys, [])
            Just n  -> (Just task, take n ys, drop (n + 1) ys)
    in Tasks (children task)  (elem:xs)

-- | go out of actual task list and go one level up
goOut :: Tasks -> Tasks
goOut (Tasks a ((Nothing,[],[]):[]))   = Tasks a ((Nothing,[],[]):[])
goOut (Tasks _ ((Just a, b, c):xs))     =
    let actual = b ++ a:c
        rest = case xs of
            (Nothing, [],[]):_ -> (Nothing, [],[]):[]
            (Just n, w,v):ks   -> (Just n { children = actual }, w,v):ks
    in
        Tasks actual rest

-- | Add Task to Tasks
addTask :: Task -> Tasks -> Tasks
addTask t (Tasks a ((Nothing, [],[]):xs))   = Tasks (t:a) ((Nothing, [],[]):xs)
addTask t (Tasks a ((Just parent, b,c):bs)) = Tasks (t:a) ((Just parent {
    children = t:(children parent)},b,c):bs)

-- | Add Tasks to Tasks
addTasks :: [Task] -> Tasks -> Tasks
addTasks t (Tasks a ((Nothing , b,c):bs))   = Tasks (t++a) ((Nothing,b,c):bs)
addTasks t (Tasks a ((Just parent,b,c):bs)) =
    Tasks (t ++ a) ((Just parent { children = t ++ (children parent)}, b,c):bs)

-- | removes Task from Tasks
removeTask :: Int -> Tasks -> Tasks
removeTask index tasks
    | index < 0                    = tasks
    | index > (length $ actual tasks) = tasks
    | otherwise                    =
        let (ys,zs) = splitAt index (actual tasks)
            c = ys ++ (tail zs)
            rest = case (history tasks) of
                (Nothing, _, _):xs -> (Nothing, [], []):xs
                (Just p, x, y):xs -> (Just p { children = c } , x , y):xs
        in Tasks c rest




lastTasks :: Tasks -> [Task]
lastTasks = actual . goOut

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

data Line = Line { tabs :: Int, txt :: String }  | NewLine deriving (Show,Eq)

-- | creates a list of tasks out out of a list of String (the format must be the same created by
-- the renderToStrings function)
stringToTasks :: [String] -> Tasks
stringToTasks = parseStringToLines
    >>> groupLines
    >>> foldl appendLine emptyTasks
    >>> unroll


parseStringToLines :: [String] -> [Line]
parseStringToLines str =
    let numberOfSpaces (' ':xs) = 1 + numberOfSpaces xs
        numberOfSpaces _        = 0
    in
        map (\l ->
            let n = numberOfSpaces l
                rest = drop n l
            in
                if rest == [] then
                    NewLine
                else
                    Line n rest
            ) str

groupLines :: [Line] -> [[Line]]
groupLines = filter (/= []) . snd . mapAccumL stacker []
    where
        stacker (NewLine:xs) NewLine = ([],reverse xs)
        stacker xs x                 = (x:xs,[])


appendLine :: Tasks -> [Line] -> Tasks
appendLine tasks lns = let (n,t) = linesToTask lns
                           d     = depth tasks
                in addTask t $ goToLevel (d - n) tasks

-- | move up or down.
-- depth < 0 means goOut
goToLevel :: Int -> Tasks -> Tasks
goToLevel 0 tasks = tasks
goToLevel n tasks
    | n < 0     = goToLevel (n + 1) $ goOut tasks
    | otherwise = case (getNextActiveTask tasks) of
        Just t -> goToLevel (n - 1) $ goInto t tasks
        Nothing -> tasks


-- | only needed for the goInto in the parsing
getNextActiveTask :: Tasks -> Maybe Task
getNextActiveTask (Tasks [] _)  = Nothing
getNextActiveTask (Tasks x _)   = Just . last $ x

-- | depth of the actual focused tasks 0 for top level
depth :: Tasks -> Int
depth (Tasks _  xs) = (length xs) - 1


linesToTask :: [Line] -> (Int,Task)
linesToTask []              = (0,Comment "Unknown" [])
linesToTask (NewLine:xs)    = linesToTask xs
linesToTask ((Line n t):xs) = (n, Comment (unlines . map lineToString $ (Line n t):xs ) [])
    where
        lineToString (Line n t) = t
        lineToString NewLine    = "\n"

-- | unrolls the tasks untill we have the toplevel
asList :: Tasks -> [Task]
asList = actual . unroll

-- | unrolls the tasks untill we have the toplevel
unroll :: Tasks -> Tasks
unroll (Tasks a ((Nothing,[],[]):xs)) = Tasks a ((Nothing,[],[]):[])
unroll f                       = unroll . goOut $ f
