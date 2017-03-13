module Snake.Types where
  data Orientation = North | East | South | West deriving (Show, Eq, Enum)

  data GameStatus = Running | Paused | Finished deriving (Show, Eq)

  togglePause Finished = Finished
  togglePause Running = Paused
  togglePause Paused = Running
