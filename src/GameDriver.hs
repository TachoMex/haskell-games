module GameDriver where
  import Control.Monad (join)
  import Types
  import Graphics.Rendering.OpenGL hiding (Color)
  import Graphics.UI.GLUT hiding (Color)

  import Data.IORef

  class GameDriver a where
    width        :: a -> Int
    height       :: a -> Int
    marginSize   :: a -> Int
    blockSize    :: a -> Int
    windowWidth  :: a -> Int
    windowWidth a = width a * blockSize a + 2 * marginSize a
    windowHeight :: a -> Int
    windowHeight a = height a * blockSize a + 2 * marginSize a
    render :: a -> [(Point, Color)]
    keyboardListener :: Char -> Position -> a -> a
    delay :: a -> Int
    nextFrame :: a -> IO a
    name :: a -> String
    newGame :: IO a

  drawBlock a ((x, y), (r, g, b)) = do
    color $ Color3 r g b
    rect (Vertex2 xb yb) (Vertex2 xe ye)
    where
      xb = fromIntegral $ marginSize a + x * blockSize a:: GLfloat
      yb = fromIntegral $ marginSize a + y * blockSize a:: GLfloat
      xe = xb + fromIntegral (blockSize a) :: GLfloat
      ye = yb + fromIntegral (blockSize a) :: GLfloat

  display :: (GameDriver a) => IORef a -> IO ()
  display gameRef = do
    game <- get gameRef
    clear [ColorBuffer]
    let blocks = render game
    mapM_ (drawBlock game) blocks
    renderPrimitive Lines $ do
      color $ Color3 1.0 1.0 (1.0 :: GLfloat)
      mapM_ (drawLineV game) [marginSize game, (marginSize game + blockSize game)..(windowWidth game - marginSize game)]
      mapM_ (drawLineH game) [marginSize game, (marginSize game + blockSize game)..(windowHeight game - marginSize game)]
    swapBuffers
    where
      drawLineV :: (GameDriver a) => a -> Int -> IO ()
      drawLineV g i = do
        let p = fromIntegral i :: GLfloat
        vertex $ Vertex2 p (fromIntegral $ marginSize g)
        vertex $ Vertex2 p (fromIntegral $ windowHeight g - marginSize g)
      drawLineH :: (GameDriver a) => a -> Int -> IO ()
      drawLineH g i = do
        let p = fromIntegral i :: GLfloat
        vertex $ Vertex2 (fromIntegral $ marginSize g) p
        vertex $ Vertex2 (fromIntegral $ windowWidth g - marginSize g) p

  keyHandler :: (GameDriver a) => IORef a -> KeyboardCallback
  keyHandler gameRef char pos = do
    game <- get gameRef
    writeIORef gameRef $ keyboardListener char pos game

  iterateGame :: (GameDriver a) => IORef a -> IO ()
  iterateGame gameRef = do
    game <- get gameRef
    newState <- nextFrame game
    writeIORef gameRef newState
    display gameRef
    addTimerCallback (delay newState) (iterateGame gameRef)

  runGame game = do
    (progname,args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    window <- createWindow $ name game
    windowSize $= Size (fromIntegral $ windowWidth game) (fromIntegral $ windowHeight game)
    ortho2D 0 (fromIntegral $ windowWidth game) 0 (fromIntegral $ windowHeight game)
    gameRef <- newIORef game
    displayCallback $= display gameRef
    keyboardCallback $= Just (keyHandler gameRef)
    addTimerCallback 1000 (iterateGame gameRef)
    -- reshapeCallback $= Just reshape
    -- idleCallback $= Just (idle worldRef evolvef)
    mainLoop
