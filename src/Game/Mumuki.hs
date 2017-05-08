module Game.Mumuki where

import Graphics.UI.GLUT
import Data.IORef

data GameSpec a = GameSpec {
                      name :: String,
                      initialState :: a,
                      nextState :: a -> a,
                      renderState :: a -> [(GLfloat, GLfloat)] }

startGame :: GameSpec a -> IO ()
startGame spec = do
  (_progName, _args) <- getArgsAndInitialize
  _window            <- createWindow . name $ spec
  state              <- newIORef . initialState $ spec
  displayCallback    $= display spec state
  schedule 1000 (nextTick spec state)
  mainLoop

display :: GameSpec a -> IORef a -> DisplayCallback
display spec state = do
  clear [ColorBuffer]
  renderPrimitive Quads $ do
     currentState <- get state
     mapM_ renderVertex . renderState spec $ currentState
  flush

nextTick :: GameSpec a -> IORef a -> TimerCallback
nextTick spec state = do
  state $~! (nextState spec)
  postRedisplay Nothing

schedule :: Timeout -> TimerCallback -> IO ()
schedule timeout callback = addTimerCallback timeout callback'
                          where callback' = callback >> addTimerCallback timeout callback'

renderVertex (x, y) = vertex $ Vertex3 x y 0
