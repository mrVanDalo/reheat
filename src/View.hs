{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  VtyView
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

module View (
    main
) where

import System.Exit
import Graphics.Vty.Widgets.All
import Graphics.Vty.Attributes
import Graphics.Vty
import Control.Monad
import System.IO
import System.Directory

import System.Console.GetOpt
import Control.Monad
import System.Environment
import System.Process


import Data.Char
import Task
import Data.IORef

import Context

import qualified Data.Text as T


main = do
    opts <- parseOptions
    context <- createContext $ ioFile opts
    execMain context { editorType = (e opts) }


{- =======================================================

   option parsing here

   =======================================================  -}

data Options = Options {
    ioFile :: FilePath,
    e      :: EditorType
    }


options :: [ OptDescr (Options -> IO Options) ]
options =
    [   Option "i" ["input"]
            (ReqArg
                (\arg opt -> return opt { ioFile = arg })
                "FILE")
            "Input and Output File",
        Option "e" ["editor"]
            (ReqArg
                (\arg opt -> return opt { e = case (map toLower arg) of
                    "vim"     -> Vim
                    "nano"    -> Nano
                    "emacs"   -> Emacs
                    otherwise -> InternalEditor
                    })
                "vim or nano or emacs or internal")
            "your favorit editor",
        Option "h" ["help"]
            (NoArg
                (\_ -> do
                    prg <- getProgName
                    hPutStrLn stderr (usageInfo prg options)
                    exitWith ExitSuccess))
            "Show help"
    ]

startOptions = Options ".todo.rht" InternalEditor

-- | parses options from command line
parseOptions = do
    args <- getArgs

    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args

    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions

    return opts


{- =======================================================

   draw UI here

   =======================================================  -}
type ContextSwitcher = IO ()

-- | create View and Run loop
execMain context = do
    let lList    = leftList context
    box <- (bordered $ bread context) <--> bordered lList <++>
        (bordered (description context) <--> bordered (rightList context))

    fg  <- newFocusGroup
    addToFocusGroup fg lList

    editor <- multiLineEditWidget
    eFg    <- newFocusGroup
    addToFocusGroup eFg editor
    d      <- plainText "Header" <--> return editor >>= withBoxSpacing 1

    c <- newCollection
    switchToMain   <- addToCollection c box fg
    switchToDialog <- addToCollection c d eFg

    editor `onKeyPressed` \_ key _ -> case key of
        KEsc -> editorExitAndSave editor context switchToMain
        _ -> return False

    lList  `onKeyPressed` \this key whatever -> case key of
        KASCII 'q' -> closeApp context
        KEsc       -> closeApp context
        KASCII 'a' -> createNewTask context switchToDialog
        KASCII 'j' -> manoverDown context
        KASCII 'J' -> manoverSwap Down context
        KASCII 'k' -> manoverUp context
        KASCII 'K' -> manoverSwap Up context
        KASCII 'd' -> deleteTask context
        KRight     -> manoverRight context
        KASCII 'l' -> manoverRight context
        KASCII 'o' -> manoverLeft context
        KLeft      -> manoverLeft context
        _ -> return False
    lList  `onSelectionChange` onListSelectionChanged context

    updateLeftList context
    runUi c defaultContext

{- =======================================================

   editor functions here

   =======================================================  -}

createNewTask :: Context -> ContextSwitcher -> IO Bool
createNewTask context switch =
    let e = editorType context
    in  do
        foo e context switch
        return True
        where
            foo Vim context _  = do
                let filePath = "/tmp/foo"
                system $ "vim" ++ " " ++ filePath
                exist <- doesFileExist filePath
                case exist of
                    True -> do
                        withFile filePath ReadMode $ (\h -> do
                            text <- hGetContents h
                            save text context switch
                            return True
                            )
                    otherwise -> do
                        return True

            foo _ _ switch = do
                switch
                return True

save :: String -> Context -> ContextSwitcher -> IO ()
save "" context switch   = do ; switch
save text context switch = do
    appendTask context (Comment text [])
    switch


-- | exit editor saves the task and switches back to main view
editorExitAndSave editor context switchToMain = do
    text <- getEditText editor
    case text of
        "" ->  do
            switchToMain
            return True
        _ -> do
            appendTask context (Comment (T.unpack text) [])
            setEditText editor ""
            focus editor
            switchToMain
            return True



{- =======================================================

   manipulation stuff here

   =======================================================  -}

onListSelectionChanged context event =
    case event of
        SelectionOff -> do
            clearList $ rightList context
        SelectionOn _ task renderedTask -> do
            clearList $ rightList context
            appendTasksToList (rightList context) (reverse (children task))
            setText (description context) $ T.pack (text task)

deleteTask context = do
    item <- getSelected $ leftList context
    case item of
        Nothing -> return True
        Just (itemNr, itemElem) -> do
            full <- getListSize $ leftList context
            -- unappendTask (full - 1 - itemNr) tasks leftList
            unappendTask itemNr context
            return True

closeApp context = do
    writeToFile (filePath context) (tasks context)
    exitSuccess
    return True

data Direction = Up | Down

manoverSwap direction context = do
    item <- getSelected $ leftList context
    case item of
        Nothing -> return True
        Just (itemNr, itemElem) -> do
            swapTasks' itemNr (other direction itemNr) context
            setSelected (leftList context) (other direction itemNr)
            return True
    where
        other Up   a = a - 1
        other Down a = a + 1



manoverRight context = do
    item <- getSelected $ leftList context
    case item of
        Nothing -> return True
        Just (itemNr, itemElem) -> do
            moveInto (fst itemElem) context
            return True

manoverLeft context = do
    moveOut context
    return True

manoverDown context = do
    scrollDown $ leftList context
    return True

manoverUp context = do
    scrollUp $ leftList context
    return True
