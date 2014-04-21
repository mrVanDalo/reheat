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
    mainLoop
) where


import UI.NCurses

mainLoop :: IO ()
mainLoop = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
        myBorder
        moveCursor 1 10
        drawString "Hello world!"
        moveCursor 3 10
        drawString "(press q to quit)"
        moveCursor 0 0
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop

myBorder = drawBorder l r u d lt rt ld rd
    where
        glyph a = Just $ Glyph a []
        l = glyph 'a'
        r = glyph 'a'
        u = glyph 'a'
        d = glyph 'a'
        lt = glyph 'a'
        rt = glyph 'a'
        ld = glyph 'a'
        rd = glyph 'a'


