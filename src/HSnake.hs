module HSnake
  ( game
  , drawNode
  ) where

import Apple
import Control.Concurrent
import Data.Word as W
import Direction as D
import Node
import Snake
import UI.HSCurses.Curses

gameOver = False

fps = 30

game :: IO ()
game = do
  initScr
  initCurses
  timeout 1
  cBreak True
  cursSet CursorInvisible
  echo False
  (termX, termY) <- scrSize
  let sn = snake (termY `div` 2) (termX `div` 2) D.Right 10
  apple <- newApple (termX, termY) sn
  score <- play (KeyUnknown 0) stdScr apple sn 0
  endWin
  putStrLn $ "Game over! Final Score: " ++ (show score)

play :: Key -> Window -> Apple -> Snake -> Int -> IO (Int)
play k scr ap (d1, sn) score =
  if gameOver || k == KeyChar 'q'
    then return score
    else do
      bounds <- scrSize
      let newSnake = wrap (moveSnake (d1, sn) newDirection) bounds
      let col = nodesCollide ap (head sn)
      nextapp <- if col
         then do
            newapple <- newApple bounds newSnake
            drawNode '@' scr newapple
            return newapple
         else do
            drawNode '@' scr ap
            return ap
      let newscore = if col then score + 1 else score
      drawSnake newSnake scr '#'
      threadDelay $ 1000000 `div` fps
      input <- getch
      play (decodeKey input) scr nextapp newSnake newscore
  where
    newDirection =
      case k of
        KeyChar 'w' -> D.Up
        KeyChar 's' -> D.Down
        KeyChar 'a' -> D.Left
        KeyChar 'd' -> D.Right
        _ -> d1

render :: Snake -> Apple -> Window -> IO ()
render s a scr= do
  drawSnake s scr '#'
  drawNode '@' scr a


drawNode :: Char -> Window -> Node -> IO ()
drawNode ch scr = \(x, y) -> mvWAddStr scr y x (ch : [])

drawSnake :: Snake -> Window -> Char -> IO ()
drawSnake (_, sn) scr ch = do
  mapM_ (drawNode ch scr) snake
  drawNode ' ' scr end
  where
    snake = reverse $ tail sn
    end = head $ reverse sn
