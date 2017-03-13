module Snake.Input where
  data Input = Input {
    pause :: Bool,
    leftArrow :: Bool,
    rightArrow :: Bool
  } deriving (Show)

  emptyInput = Input {
    pause = False,
    leftArrow = False,
    rightArrow = False
  }
