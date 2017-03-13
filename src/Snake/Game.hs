module Snake.Game where
  import Snake.Board
  import Snake.Snake
  import Snake.Input
  import Snake.Types

  import System.Random

  import Types
  import GameDriver as GD



  data Game = Game {
    snake :: Snake,
    board :: Board,
    score :: Int,
    lives :: Int,
    status :: GameStatus,
    input :: Input
  } deriving (Show)


  cleanInput g = g { input = emptyInput }
  processPause g@(Game snk brd scr liv stts inpt) = return $ cleanInput g { status = togglePause stts }
  processLeft g@(Game snk brd scr liv stts inpt) = stepForward $ g { snake = turnSnakeLeft snk }
  processRight g@(Game snk brd scr liv stts inpt) = stepForward $ g { snake = turnSnakeRight snk }

  stepForward :: Game -> IO Game
  stepForward g@(Game snk brd scr liv stts inpt)
    | not $ canMove snk brd = processCrashing g
    | snakeHead snk == foodPosition brd = do
        let snk' = move $ grow snk
        newFood <- generateNewFood brd snk'
        return $ cleanInput g { snake = snk', score = scr + 100, board = brd { foodPosition = newFood } }
    | otherwise = return $ cleanInput g { snake = move snk }

  processCrashing :: Game -> IO Game
  processCrashing g
    | lives g == 0 = return g { status = Finished }
    | otherwise = do
      g' <- newGame :: IO Game
      return g' { score = score g, lives = l}
      where
        l = lives g - 1
        s = score g

  instance GameDriver Game where
    name _ = "Snake"
    width _ = 20
    height _ = 15
    marginSize _ = 40
    blockSize _ = 40
    keyboardListener char _ game =
        case char of
          'a' -> game { input = inpt { leftArrow = True, rightArrow = False } }
          'd' -> game { input = inpt { rightArrow = True, leftArrow = False } }
          ' ' -> game { input = inpt { pause = True } }
          _ -> game
        where
          inpt = input game
    delay g = max 100 (1000  - score g `div` 5 )
    render g = f : tail snke ++ [hd]
      where
        hd = (snakeHead $ snake g, orange)
        f = (foodPosition . board $ g, red)
        snke = fmap snkeBlock . snakeTail $ snake g
        snkeBlock s = (s, green)
    newGame = do
      let w = GD.width (undefined :: Game)
          h = GD.height (undefined :: Game)
      o' <- randomRIO (0, 3) :: IO Int
      (x, y) <- genPoint (w - 4) (h - 4)
      let o = iterate succ North !! o'
          s = newSnake (x + 2, y + 2) o
      b <- newBoard w h s
      return Game{
        snake = s,
        board = b,
        score = 0,
        lives = 2,
        status = Running,
        input = emptyInput
      }
    nextFrame g@(Game _ _ _ _ stts inpt)
      | pause inpt = processPause g
      | stts == Paused = return $ cleanInput g
      | stts == Finished = return $ cleanInput g
      | leftArrow inpt = processLeft g
      | rightArrow inpt = processRight g
      | otherwise = stepForward g
