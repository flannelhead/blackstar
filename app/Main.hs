{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Directory
import Vision.Image
import Vision.Image.Storage.DevIL
import Linear hiding (lookAt)

import Raytracer

myScene :: Scene
myScene = Scene { stepSize = 0.16
                , nSteps = 250
                , camera = myCamera }

myCamera :: Camera
myCamera = Camera { position = V3 0 1 (-20)
                  , lookAt = V3 0 0 0
                  , upVec = V3 0.2 1 0
                  , fov = 1.5
                  , resolution = (1024, 786) }

main :: IO ()
main = do
    etex <- load Autodetect "texture.jpg"
    case etex of
        Right (tex :: RGB) -> do
            img <- computeP $ render myScene tex
            removeFile "out.png"
            _ <- save PNG "out.png" ((convert img) :: RGB)
            return ()
        _ -> return ()
