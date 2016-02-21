module Main where

import Geometry
import Raytracer
import Vision.Image
import Vision.Image.Storage.DevIL

myScene :: Scene
myScene = Scene { stepSize = 0.1
                , nSteps = 250
                , toCartesian = schwarzToCartesian
                , fromCartesian = cartesianToSchwarz
                , fgeodesic = schwarzGeodesic
                , camera = myCamera }

myCamera :: Camera
myCamera = Camera { position = [20, 0, 1]
                  , lookAt = [0, 0, 0]
                  , upVec = [0, 0.2, 1]
                  , fov = 1.5
                  , resolution = (800, 450) }

main :: IO ()
main = do
    img <- computeP $ raytrace myScene
    _ <- save Autodetect "out.png" ((convert img) :: RGB)
    return ()
