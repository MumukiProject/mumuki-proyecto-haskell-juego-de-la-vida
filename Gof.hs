import Graphics.UI.GLUT
import Data.IORef

data Cell = Cell Int Int deriving Show




initialCells :: [Cell]
initialCells = [Cell 0 4, Cell 5 4, Cell 0 10, Cell 3 3, Cell 2 5]

elJuegoDeLaVida ((Cell x y):xs) = (Cell y x):xs




main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Juego de la vida"
  cells <- newIORef initialCells
  displayCallback $= display cells
  schedule 1000 (nextTick cells)
  mainLoop
 
display :: IORef [Cell] -> DisplayCallback
display cells = do 
  clear [ColorBuffer]
  renderPrimitive Quads $ do
     currentCells <- get cells
     mapM_ renderVertex . concatMap drawCell $ currentCells
  flush

nextTick :: IORef [Cell] -> TimerCallback
nextTick cells = do
  cells $~! elJuegoDeLaVida
  postRedisplay Nothing

schedule :: Timeout -> TimerCallback -> IO ()
schedule timeout callback = addTimerCallback timeout callback'
                          where callback' = callback >> addTimerCallback timeout callback'


drawCell :: Cell -> [(GLfloat, GLfloat)]
drawCell (Cell x y) = [(xCenter + delta, yCenter + delta),
                       (xCenter + delta, yCenter - delta),
                       (xCenter - delta, yCenter - delta),
                       (xCenter - delta, yCenter + delta)] 
                   where
                      xCenter  = (fromIntegral x - 5.0) * 0.1
                      yCenter  = (fromIntegral y - 5.0) * 0.1
                      size     = 0.08
                      delta    = size / 2
                  

renderVertex (x, y) = vertex $ Vertex3 x y 0
