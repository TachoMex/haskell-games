module Snake.Board where
  import Snake.Snake
  import Snake.Types
  import Types

  data Board = Board {
    widht :: Int,
    height :: Int,
    foodPosition :: Point
  } deriving (Show)

  canMove :: Snake -> Board -> Bool
  canMove snk b = not (collition snk newPoint) && inBounds newPoint b
    where
      newPoint = snakeHead $ move snk
      inBounds (x, y) (Board w h _) = x >= 0 && y >= 0 && x < w && y < h

  generateNewFood :: Board -> Snake -> IO Point
  generateNewFood b@(Board w h _) snk = do
    newPoint <- genPoint w h
    if collition snk newPoint
        then generateNewFood b snk
        else return newPoint

  newBoard w h s = do
    let badBoard = Board w h undefined
    p <- generateNewFood badBoard s
    return badBoard { foodPosition = p }
