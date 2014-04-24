-----------------------------------------------------------------------------
--
-- Module      :  TaskView
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

module TaskView (
    mainLoop,
    main
) where

import UI.HSCurses.Curses
import Task


mainLoop = do
    initCurses
    keypad stdScr True -- make the cursor keys usable
    echo False -- disable terminal echo
    cursSet CursorInvisible
    (sizeY, sizeX) <- scrSize
    moveAbout (sizeY `div` 2) (sizeX `div` 2)
    endWin

-- | note that curses positions are (y, x) coordinates, with (0, 0) being
-- | the upmost leftmost position
moveAbout pY pX = do
    erase -- clear curses's virtual screen but don't force a redraw
    mvAddCh pY pX (castEnum '@') -- place a character in curses's virtual screen
    refresh -- copy the virtual screen to the terminal
    c <- getCh
    case c of
        KeyUp -> moveAbout (pY - 1) pX
        KeyDown -> moveAbout (pY + 1) pX
        KeyLeft -> moveAbout pY (pX - 1)
        KeyRight -> moveAbout pY (pX + 1)
        _ -> return ()

-- | Useful for transforming a Char to a ChType. Not sure if this is
-- safe outside of the 7-bit ASCII range.
castEnum = toEnum . fromEnum



main = do
    initCurses
    keypad stdScr True
    echo False
    cursSet CursorInvisible
    size <- scrSize
    taskLoop size getTasks
    -- taskLoop size []
    endWin


taskLoop (sizeX,sizeY) tasks = do
    erase
    drawTasks tasks
    refresh
    c <- getCh
    case c of
        KeyUp       -> taskLoop (sizeX,sizeY)  $ moveTask Previous tasks
        KeyDown     -> taskLoop (sizeX, sizeY) $ moveTask Next tasks
        KeyLeft     -> taskLoop (sizeX, sizeY) $ moveTask Back tasks
        KeyRight    -> taskLoop (sizeX, sizeY) $ moveTask Into tasks
        KeyChar 'q' -> return ()
        KeyChar 'Q' -> return ()
        _           -> taskLoop (sizeX, sizeY) tasks

-- | just some dummy data
getTasks :: Tasks
getTasks = [ Focused (Comment "This is a comment" []) []
    [CheckBox False "hallo" [], CheckBox True "wurst" []]]

popupWindow :: (Int,Int) -> IO Window
popupWindow (sizeX,sizeY) =
    let
        windowSize   = sizeX `div` 2
        windowHeight = sizeY `div` 2
        (a,b)        = (windowSize `div` 2, windowHeight `div` 2)
        (x,y)        = (a + windowSize, b + windowHeight)
    in
        newWin 30 30 3 3



drawTasks [] = do
    size <- scrSize
    popup <- popupWindow (100,100)
    wAddStr stdScr "no Tasks"

drawTasks (x:xs) = do
    mvWAddStr stdScr 2 2 (d x)
    where d (Focused x _ _) = text x


