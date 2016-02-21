module Main where

import Geometry
import Raytracer

myScene :: Scene
myScene = Scene { stepSize = 0.01
                , nSteps = 250
                , nRays = 20000
                , toCartesian = schwarzToCartesian
                , fromCartesian = cartesianToSchwarz
                , fgeodesic = schwarzGeodesic
                , camera = myCamera }

myCamera :: Camera
myCamera = Camera { position = [20, 0, 0]
                  , lookAt = [0, 0, 0]
                  , upVec = [0, 0.1, 1]
                  , fov = 1.5
                  , resolution = (320, 200) }

main :: IO ()
main = trace myScene
