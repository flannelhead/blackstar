{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Directory
import Vision.Image
import Vision.Image.Storage.DevIL

import Geometry
import Raytracer

myScene :: Scene
myScene = Scene { stepSize = 0.1
                , nSteps = 250
                , toCartesian = schwarzToCartesian
                , fromCartesian = cartesianToSchwarz
                , fgeodesic = schwarzGeodesic
                , camera = myCamera }

myScene' :: Scene
myScene' = Scene { stepSize = 0.1
                 , nSteps = 250
                 , toCartesian = id
                 , fromCartesian = id
                 , fgeodesic = flatGeodesic
                 , camera = myCamera }

myCamera :: Camera
myCamera = Camera { position = [1, 0, 20]
                  , lookAt = [0, 0, 0]
                  , upVec = [1, 0.2, 0]
                  , fov = 1.5
                  , resolution = (400, 225) }

myCamera' :: Camera
myCamera' = Camera { position = [0, 0, 0]
                   , lookAt = [1, 0, 0]
                   , upVec = [0, 0, 1]
                   , fov = 1.5
                   , resolution = (800, 450) }

main :: IO ()
main = do
    etex <- load Autodetect "texture.jpg"
    case etex of
        Right (tex :: RGB) -> do
            img <- computeP $ raytrace myScene tex
            removeFile "out.png"
            _ <- save PNG "out.png" ((convert img) :: RGB)
            return ()
        _ -> return ()
