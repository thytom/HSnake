module HSnake
    ( game
    ) where

import UI.HSCurses.Curses

game :: IO ()
game = do
  scr <- initScr
  initCurses
  mvWAddStr scr 0 0 "This is a test."
  refresh
  _ <- getCh
  endWin
