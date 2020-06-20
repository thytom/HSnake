module Snake where

import Direction
import Node

type Snake = (Direction, [Node])

invalidMoves :: [(Direction, Direction)]
invalidMoves =
  [ (Direction.Up, Direction.Down)
  , (Direction.Down, Direction.Up)
  , (Direction.Left, Direction.Right)
  , (Direction.Right, Direction.Left)
  ]

validMove :: Direction -> Direction -> Bool
validMove d1 d2 = all (/= (d1, d2)) invalidMoves

wrap :: Snake -> (Int, Int) -> Snake
wrap (d1, sn) (h, w) = (d1, (fixY $ fixX $ head sn) : tail sn)
  where fixX (x, y)
          | x < 0 = (w - 1, y)
          | x >= w = (0, y)
          | otherwise = (x, y)
        fixY (x, y)
          | y < 0 = (x, h - 1)
          | y >= h = (x, 0)
          | otherwise = (x, y)

moveSnake :: Snake -> Direction -> Snake
moveSnake (d1, orig) d2
  | validMove d1 d2 = (d2, newhead : newtail)
  | otherwise = (d1, orig)
  where
    newhead = applyVector (asNodeVector d2) $ head orig
    newtail = reverse $ tail $ reverse orig

growSnake :: Int -> Snake -> Snake
growSnake amt (d1, orig) = (d1, orig ++ [(0, 0) | _ <- [1..amt]])

snake :: Int -> Int -> Direction -> Int -> Snake
snake x y dir len = (dir, body)
  where body = [applyVector (x, y) (a*(-n), b*(-n)) | n <- [0..len + 1]]
        (a, b) = asNodeVector dir
