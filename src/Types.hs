module Types where
  import System.Random

  type Point = (Int, Int)
  type Color = (Double, Double, Double)

  green = (0, 1, 0) :: Color
  red = (1, 0, 0) :: Color
  orange = (1, 0.5, 0) :: Color
  white = (1, 1, 1) :: Color

  genPoint w h = do
     x <- randomRIO (0, w - 1)
     y <- randomRIO (0, h - 1)
     return (x, y)
