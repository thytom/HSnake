module Apple where

import System.Random
import Node
import Snake

type Apple = Node

newApple :: (Int, Int) -> Snake -> IO Apple
newApple (h, w) (d, sn) = do
  apple <- a
  if nodeCollidesWithAny apple sn
     then newApple (w, h) (d, sn)
     else a
      where a = do
                x <- randomRIO (0, w - 1)
                y <- randomRIO (0, h - 1)
                return (x, y)

