{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  VtyView
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module VtyView (
    main,
    main2
) where

import System.Exit
import Graphics.Vty.Widgets.All
import Graphics.Vty.Attributes
import Graphics.Vty
import Control.Monad
import System.IO
import System.Directory

import Task
import Data.IORef

import qualified Data.Text as T

-- | To be interpreted by the Main
data Action = Exit | OpenCreateTaskDialog | MoveInto Task | MoveOut | CreateTask Task

main2 = do
    let filePath = "foo"
    tasks <- readFromFile filePath
    putStr . show $ tasks
    writeToFile (filePath ++ ".tmp") =<< newIORef tasks

main = do
    let filePath = ".todo.rht"
    tasks <- newIORef =<< readFromFile filePath
    leftList <- newTaskList []
    fullText <- textWidget wrap "Welcome to Reheat"
    rightList <- newTaskList []

    box <- bordered =<< (bordered =<< plainText "[a : add] [d : delete] [i: in] [o: out]") <--> bordered leftList <++>
        (bordered fullText <--> bordered rightList)

    fg  <- newFocusGroup
    addToFocusGroup fg leftList

    editor <- multiLineEditWidget
    eFg <- newFocusGroup
    addToFocusGroup eFg editor
    pe <- plainText "Header" <--> return editor >>= withBoxSpacing 1
    (d, dFg) <- newDialog pe "Enter new Entrie"
    dialog <- centered =<< withPadding (padLeftRight 2) (dialogWidget d)

    c <- newCollection
    switchToMain <- addToCollection c box fg
    switchToDialog <- addToCollection c dialog =<< (mergeFocusGroups eFg dFg)

    editor `onKeyPressed` \this key mod -> case key of
        KEsc -> do
            switchToMain
            return True
        _ -> return False

    d `onDialogAccept` \this -> do
        text <- getEditText editor
        appendTask tasks leftList (Comment (T.unpack text) [])
        setEditText editor ""
        focus editor
        switchToMain

    d `onDialogCancel` \this -> do
        switchToMain

    leftList  `onKeyPressed` \this key whatever -> case key of
        KASCII 'q' -> do
            writeToFile filePath tasks
            exitSuccess
            return True
        KASCII 'a' -> do
            switchToDialog
            return True
        KASCII 'j' -> do
            scrollDown leftList
            return True
        KASCII 'k' -> do
            scrollUp leftList
            return True
        KASCII 'd' -> do
            item <- getSelected leftList
            case item of
                Nothing -> return True
                Just (itemNr, itemElem) -> do
                    full <- getListSize leftList
                    -- unappendTask (full - 1 - itemNr) tasks leftList
                    unappendTask itemNr tasks leftList
                    return True
        KRight     -> manoverRight tasks leftList
        KASCII 'l' -> manoverRight tasks leftList
        KASCII 'o' -> manoverLeft tasks leftList
        KLeft      -> manoverLeft tasks leftList

        _ -> return False

    leftList  `onSelectionChange` \event -> case event of
        SelectionOff -> do
            clearList rightList
        SelectionOn _ task renderedTask -> do
            clearList rightList
            appendTasksToList rightList (reverse (children task))
            setText fullText $ T.pack (text task)

    setList tasks leftList

    runUi c defaultContext

manoverRight tasks leftList = do
    item <- getSelected leftList
    case item of
        Nothing -> return True
        Just (itemNr, itemElem) -> do
            moveInto (fst itemElem) tasks leftList
            return True

manoverLeft tasks leftList = do
    moveOut tasks leftList
    return True

activeTasks :: IORef Tasks -> IO [Task]
activeTasks ref = do
    l <- readIORef ref
    return $ actual l




type TaskViewList = Widget (List Task Task)

-- | creates a new task list
newTaskList :: [Task] -> IO TaskViewList
newTaskList tasks = do
    list <- newList (green `on` black)
    appendTasksToList list tasks
    return list

-- | Append multiple tasks to list
appendTasks :: IORef Tasks -> TaskViewList -> [Task] -> IO ()
appendTasks tasks list tasksToAppend = do
    modifyIORef tasks $ addTasks tasksToAppend
    setList tasks list

-- | Append single task to list
appendTask :: IORef Tasks -> TaskViewList -> Task -> IO ()
appendTask tasks list task = do
    modifyIORef tasks $ addTask task
    setList tasks list

unappendTask :: Int -> IORef Tasks -> TaskViewList -> IO ()
unappendTask index tasks list = do
    modifyIORef tasks $ removeTask index
    setList tasks list

-- | move into task
moveInto :: Task -> IORef Tasks -> TaskViewList -> IO ()
moveInto task tasks list = do
    modifyIORef tasks $ goInto task
    setList tasks list

-- | move out of the actual task list
moveOut :: IORef Tasks -> TaskViewList -> IO ()
moveOut tasks list = do
    modifyIORef tasks goOut
    setList tasks list

setList :: IORef Tasks -> TaskViewList -> IO ()
setList tasks list = do
    clearList list
    newTasks <- activeTasks tasks
    appendTasksToList list (reverse newTasks)
    setSelected list 0
    return ()

appendTaskToList l t = appendTasksToList l [t]

appendTasksToList list tasks =
    forM_ tasks $ \t -> do
        f <- newTask t
        b <- bordered f
        (insertIntoList list t f 0)


-- | write tasks to file
writeToFile :: FilePath -> IORef Tasks -> IO ()
writeToFile fileName wRef = do
    tasks <- readIORef wRef
    withFile fileName WriteMode $ \h -> do
        hPutStr h (show tasks)

readFromFile :: FilePath -> IO Tasks
readFromFile f = do
    exist <- doesFileExist f
    foo exist f

foo :: Bool -> FilePath -> IO Tasks
foo False _ = return $ emptyTasks
foo True f  = do
    withFile f ReadMode $ \h -> do
        ls <- hGetLine h
        return $ read ls

-- | create a rendarble Task
newTask :: Task -> IO (Widget Task)
newTask t = do
    newWidget t $ \w ->
        w { render_ =  \this region ctx -> do
            elem  <- getState this
            renderTask region ctx elem
          }

-- | Task to Image Renderer
-- renderTask :: DisplayRegion -> RenderContext -> Task -> IO Image
renderTask region ctx task = do     
    let s                 = T.pack $ head . lines . text $ task
        width             = (fromEnum $ region_width region)
        (truncated, _, _) = clip1d (Phys 0) (Phys width) s
    return $ string (getNormalAttr ctx) $ T.unpack truncated
