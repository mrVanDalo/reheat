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
-- module Task where

module Task (
    -- | data
    Task(..),
    Tasks(..),
    emptyTasks,

    -- | movement
    goInto,
    goOut,

    -- | manipulation
    addTask,
    addTasks,
    removeTask,
    swapTasks,

    -- | serialization
    readTasks,
    saveTasks,

    -- | information
    breadCrumbs,
) where

import Task.Data
import Task.Modification
