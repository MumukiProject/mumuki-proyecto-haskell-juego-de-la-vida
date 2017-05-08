module JuegoDeLaVida (
    primeraGeneracion,
    siguienteGeneracion,
    Generacion(..),
    Celula(..)) where

data Celula = Celula { posicionX :: Int,
                       posicionY :: Int } deriving Show

type Generacion = [Celula]

primeraGeneracion :: Generacion
primeraGeneracion =  [ Celula 0 4,
                      Celula 5 4,
                      Celula 0 10,
                      Celula 3 3,
                      Celula 2 5 ]

siguienteGeneracion :: Generacion -> Generacion
siguienteGeneracion ((Celula x y):xs) = (Celula y x):xs
