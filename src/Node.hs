module Node where

type Node = (Int, Int)
-- Shows the difference between vectors and coordinates
type NodeVector = Node

nodesCollide :: Node -> Node -> Bool
nodesCollide (a, b) (c, d)
  | a == c && b == d  = True
  | otherwise         = False

nodeCollidesWithAny :: Node -> [Node] -> Bool
nodeCollidesWithAny a xs = any (nodesCollide a) xs

applyVector :: NodeVector -> Node -> Node
applyVector (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
