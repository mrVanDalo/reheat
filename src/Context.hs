-----------------------------------------------------------------------------
--
-- Module      :  Context
-- Copyright   :  GPL v3
-- License     :  AllRightsReserved
--
-- Maintainer  :  Ingolf Wagner <palipalo9@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | Holds all the objects and functions for manipulating them.
-- It should be used as a real programm while View should wire this module to create
-- the UI.
--
-----------------------------------------------------------------------------

module Context where

import Graphics.Vty.Widgets.All
import Graphics.Vty.Attributes
import Graphics.Vty
import Control.Monad
import System.IO
import System.Directory
import Data.IORef
import qualified Data.Text as T

import Task

type TaskViewList         = Widget (List Task Task)
type TaskDescription      = Widget FormattedText
type BreadCrumbWidget     = Widget BreadCrumb
type BreadCrumb           = [Task]


data Context = Context {
    description :: TaskDescription,
    bread       :: BreadCrumbWidget,
    leftList    :: TaskViewList,
    rightList   :: TaskViewList,
    tasks       :: IORef Tasks
    }

-- | create Context out of a file
createContext :: FilePath -> IO Context
createContext filePath = do
    tasks      <- newIORef =<< readFromFile filePath
    leftList   <- newTaskList []
    fullText   <- textWidget wrap $ T.pack "Welcome to Reheat"
    bread      <- newBreadCrumb []
    rightList  <- newTaskList []
    return $ Context fullText bread leftList rightList tasks

-- | creates a new task list
newTaskList :: [Task] -> IO TaskViewList
newTaskList tasks = do
    list <- newList (green `on` black)
    appendTasksToList list tasks
    return list


-- | Append single task to list
appendTask :: Context -> Task -> IO ()
appendTask context task = do
    modifyIORef (tasks context) $ addTask task
    updateLeftList context

-- | updates left list in context
updateLeftList :: Context -> IO ()
updateLeftList context = do
    t <- readIORef $ tasks context
    let ts = actual t
    setList ts (leftList context)
    updateBreadCrumbs context
    return ()

-- | unwrapps the actual task list from the context
actualTaskList :: Context -> IO [Task]
actualTaskList context = do
    t <- readIORef $ tasks context
    return $ actual t

unappendTask :: Int -> Context -> IO ()
unappendTask index context = do
    modifyIORef (tasks context) $ removeTask index
    updateLeftList context

-- | move into task
moveInto :: Task -> Context -> IO ()
moveInto task context = do
    modifyIORef (tasks context) $ goInto task
    updateLeftList context



-- | move out of the actual task list
moveOut :: Context -> IO ()
moveOut context = do
    modifyIORef (tasks context) goOut
    updateLeftList context

-- | clear list and set tasks to that value
setList :: [Task] -> TaskViewList -> IO ()
setList tasks list = do
    clearList list
    appendTasksToList list (reverse tasks)
    setSelected list 0
    return ()

-- | returns the active tasks right now
activeTasks :: Context -> IO [Task]
activeTasks context = do
    l <- readIORef $ tasks context
    return $ actual l


-- | append some tasks to the list
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
        hPutStr h (show $ saveTasks tasks)

-- | read from file
readFromFile :: FilePath -> IO Tasks
readFromFile f = do
    exist <- doesFileExist f
    case exist of -- | if is not working here
        False -> do return $ emptyTasks
        True  -> do
            withFile f ReadMode $ \h -> do
                ls <- hGetLine h
                return $ readTasks $ read ls


-- | create a rendarble Task
newTask :: Task -> IO (Widget Task)
newTask t = do
    newWidget t $ \w ->
        w { render_ =  \this region ctx -> do
            elem  <- getState this
            renderTask region ctx elem
          }



-- | Task to Image Renderer
renderTask :: DisplayRegion -> RenderContext -> Task -> IO Image
renderTask region ctx task = do     
    let header            = T.pack $ head . lines . text $ task
        width             = (fromEnum $ region_width region)
        (truncated, _, _) = clip1d (Phys 0) (Phys width) header
    return $ string (getNormalAttr ctx) $ T.unpack truncated



-- | bread crumbs here


updateBreadCrumbs :: Context -> IO ()
updateBreadCrumbs ctx = do
    t <- readIORef $ tasks ctx
    let newCrumbs = breadCrumbs t
        crumbObject = bread ctx
    setBreadCrumbs newCrumbs crumbObject
    return ()


setBreadCrumbs :: [Task] -> Widget BreadCrumb -> IO ()
setBreadCrumbs t bread = do
    updateWidgetState bread (\a -> t)

newBreadCrumb :: [Task] -> IO (Widget BreadCrumb)
newBreadCrumb tasks = newWidget tasks $ \w ->
    w { render_ = \this region ctx -> do
        listOfTasks <- getState this
        renderBreadCrumb region ctx listOfTasks
      }

renderBreadCrumb :: DisplayRegion -> RenderContext -> BreadCrumb -> IO Image
renderBreadCrumb region ctx bread = do

    return $ string (getNormalAttr ctx) $ renderBread bread ""
    where
        width = (fromEnum $ region_width region)
        renderBread bread acc
            | width     <= (length acc)= drop ((length acc) - width) acc
            | bread == [] && acc == [] = "/"
            | bread == []              = acc
            | otherwise                =
                let header            = T.pack $ head . lines . text . head $ bread
                    (truncated, _, _) = clip1d (Phys 0) (Phys width) header
                    str               = T.unpack truncated
                in renderBread (tail bread) (str ++ " > " ++ acc)
