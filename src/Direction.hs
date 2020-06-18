module Direction where

import Node

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Eq)

asNodeVector :: Direction -> Node
asNodeVector Direction.Up     = ( 0, -1)
asNodeVector Direction.Down   = ( 0,  1)
asNodeVector Direction.Left   = (-1,  0)
asNodeVector Direction.Right  = ( 1,  0)
