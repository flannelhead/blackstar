module Main where

import Geometry
import Raytracer
import Data.Array.Repa

myScene :: Scene
myScene = Scene { stepSize = 0.01
                , nSteps = 250
                , toCartesian = schwarzToCartesian
                , fromCartesian = cartesianToSchwarz
                , fgeodesic = schwarzGeodesic
                , camera = myCamera }

myCamera :: Camera
myCamera = Camera { position = [20, 0, 0]
                  , lookAt = [0, 0, 0]
                  , upVec = [0, 0.1, 1]
                  , fov = 1.5
                  , resolution = (1920, 1080) }

main :: IO ()
main = do
    -- _ <- return $! computeUnboxedS $ raytrace myScene
    _ <- computeUnboxedP $ raytrace myScene
    return ()
