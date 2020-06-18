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

moveSnake :: Snake -> Direction -> Snake
moveSnake (d1, orig) d2
  | validMove d1 d2 = (d2, newhead : newtail)
  | otherwise = (d1, orig)
  where
    newhead = applyVector (asNodeVector d2) $ head orig
    newtail = reverse $ tail $ reverse orig

growSnake :: Snake -> Int -> Snake
growSnake (d1, orig) amt = (d1, orig ++ [(0, 0) | _ <- [1..amt]])

snake :: Int -> Int -> Direction -> Int -> Snake
snake x y dir len = undefined
