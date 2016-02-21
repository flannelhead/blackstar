module Main where

import Geometry
import Raytracer

myScene :: Scene
myScene = Scene { stepSize = 0.01
                , nSteps = 250
                , nRays = 20000
                , toCartesian = schwarzToCartesian
                , fromCartesian = cartesianToSchwarz
                , fgeodesic = schwarzGeodesic }

main :: IO ()
main = trace myScene
