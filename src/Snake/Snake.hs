module Snake.Snake where
  import Snake.Types
  import Types

  turnLeft North = West
  turnLeft o = pred o

  turnRight West = North
  turnRight o = succ o

  data Snake = Snake {
    snakeHead :: Point,
    snakeTail :: [Point],
    facing :: Orientation
  } deriving (Show)

  newSnake p = Snake p [p]

  move :: Snake -> Snake
  move snk@(Snake hd tl fc) = Snake newHead newTail fc
    where
      newHead = nextPoistion snk
      newTail = newHead : init tl

  nextPoistion :: Snake -> Point
  nextPoistion (Snake hd _ fc) = movePoint fc hd

  turnSnakeLeft (Snake hd tl fc) = Snake hd tl (turnLeft fc)
  turnSnakeRight (Snake hd tl fc) = Snake hd tl (turnRight fc)

  grow (Snake hd tl fc) = Snake hd (hd:tl) fc

  collition :: Snake -> Point -> Bool
  collition snk p = p `elem` snakeTail snk

  movePoint North (x, y) = (x, y + 1)
  movePoint East  (x, y) = (x + 1, y)
  movePoint South (x, y) = (x, y - 1)
  movePoint West  (x, y) = (x - 1, y)
