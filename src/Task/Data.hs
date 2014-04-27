-----------------------------------------------------------------------------
--
-- Module      :  Task.Data
-- Copyright   :  GPL v3
-- License     :  AllRightsReserved
--
-- Maintainer  :  Ingolf Wagner <palipalo9@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Task.Data where

-- | The empty Task version.
-- use this to create an Empty Tasks Element
emptyTasks :: Tasks
emptyTasks = Tasks []  []

data Task = Comment { text :: String , children :: [Task] } deriving (Eq,Show)
data Tasks = Tasks { actual :: [Task], history ::  [FTask] } deriving (Show)
data FTask = FTask { focused :: Task, prev  :: [Task], next :: [Task] } deriving (Show)

-- | data types for saveing
-- don't use getter and setter names, to not interfere with the
-- original data objects. These types are just for saving and nothing more
data SaveTasks = Version1 [SaveTask] [SaveFTask] deriving (Show,Read)
data SaveTask = Task1 String [SaveTask] deriving (Show,Read)
data SaveFTask = FTask1 SaveTask [SaveTask] [SaveTask] deriving (Show,Read)

readTasks :: SaveTasks -> Tasks
readTasks (Version1 sTasks sFTasks) = Tasks (map readTask sTasks) (map readFTask sFTasks)

saveTasks :: Tasks -> SaveTasks
saveTasks (Tasks tasks fTasks) = Version1 (map saveTask tasks) (map saveFTask fTasks)

readTask :: SaveTask -> Task
readTask (Task1 a rest) = Comment a (map readTask rest)

saveTask :: Task -> SaveTask
saveTask (Comment a h) = Task1 a (map saveTask h)

readFTask :: SaveFTask -> FTask
readFTask (FTask1 f l r) = FTask (readTask f) (map readTask l) (map readTask r)

saveFTask :: FTask -> SaveFTask
saveFTask (FTask f l r) = FTask1 (saveTask f) (map saveTask l) (map saveTask r)
