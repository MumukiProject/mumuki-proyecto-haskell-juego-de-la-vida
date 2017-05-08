module Main (main) where

import JuegoDeLaVida as Juego
import Graphics.UI.GLUT
import Game.Mumuki


main :: IO ()
main = startGame $ GameSpec { name = "Juego de la Vida",
                              initialState = Juego.primeraGeneracion,
                              nextState = Juego.siguienteGeneracion,
                              renderState = concatMap dibujarCelula }

dibujarCelula :: Celula -> [(GLfloat, GLfloat)]
dibujarCelula (Celula x y) = [(xCenter + delta, yCenter + delta),
                       (xCenter + delta, yCenter - delta),
                       (xCenter - delta, yCenter - delta),
                       (xCenter - delta, yCenter + delta)]
                   where
                      xCenter  = (fromIntegral x - 5.0) * 0.1
                      yCenter  = (fromIntegral y - 5.0) * 0.1
                      size     = 0.08
                      delta    = size / 2
