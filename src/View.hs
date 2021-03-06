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

import Task
import Data.IORef

import Context

import qualified Data.Text as T

-- | To be interpreted by the Main
data Action = Exit | OpenCreateTaskDialog | MoveInto Task | MoveOut | CreateTask Task

main = do
    let filePath = ".todo.rht"

    context <- createContext filePath

    let lList    = leftList context
        fullText = description context
        rList    = rightList context
        b        = bread context

    box <- (bordered b) <--> bordered lList <++>
        (bordered fullText <--> bordered rList)

    fg  <- newFocusGroup
    addToFocusGroup fg lList

    editor <- multiLineEditWidget
    eFg    <- newFocusGroup
    addToFocusGroup eFg editor
    d      <- plainText "Header" <--> return editor >>= withBoxSpacing 1

    c <- newCollection
    switchToMain   <- addToCollection c box fg
    switchToDialog <- addToCollection c d eFg

    editor `onKeyPressed` \this key mod -> case key of
        KEsc -> do
            switchToMain
            return True
        _ -> return False

    editor `onKeyPressed` \_ key _ -> case key of
        KEsc -> do
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
        _ -> do
            return False

    lList  `onKeyPressed` \this key whatever -> case key of
        KASCII 'q' -> closeApp filePath context
        KEsc       -> closeApp filePath context
        KASCII 'a' -> do
            switchToDialog
            return True
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

    lList  `onSelectionChange` \event -> case event of
        SelectionOff -> do
            clearList rList
        SelectionOn _ task renderedTask -> do
            clearList rList
            appendTasksToList rList (reverse (children task))
            setText fullText $ T.pack (text task)

    updateLeftList context

    runUi c defaultContext


deleteTask context = do
    item <- getSelected $ leftList context
    case item of
        Nothing -> return True
        Just (itemNr, itemElem) -> do
            full <- getListSize $ leftList context
            -- unappendTask (full - 1 - itemNr) tasks leftList
            unappendTask itemNr context
            return True

closeApp filePath context = do
    writeToFile filePath (tasks context)
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
