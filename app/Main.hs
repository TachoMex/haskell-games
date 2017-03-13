module Main where
  import GameDriver
  import qualified Snake.Game as Snake

  main :: IO ()
  main = do
    game <- newGame :: IO Snake.Game
    runGame game
