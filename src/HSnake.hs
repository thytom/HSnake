module HSnake
    ( game
    , drawNode
    ) where

import UI.HSCurses.Curses
import Data.Word as W
import Node

game :: IO ()
game = do
  initScr
  initCurses
  drawNode stdScr (0, 0) '#'
  refresh
  _ <- getCh
  endWin

drawNode :: Window -> Node -> Char -> IO ()
drawNode scr (x, y) ch = mvWAddStr scr y x [ch]
