{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Directory
import Control.Monad
import Control.DeepSeq
import Vision.Image hiding (map)
import Vision.Image.Storage.DevIL
import Linear hiding (lookAt)

import Raytracer
import StarMap

myScene :: Scene
myScene = Scene { stepSize = 0.15
                , nSteps = 250
                , camera = myCamera }

myCamera :: Camera
myCamera = Camera { position = V3 0 1 (-20)
                  , lookAt = V3 0 0 0
                  , upVec = V3 0.2 1 0
                  , fov = 1.5
                  , resolution = (1280, 720) }

main :: IO ()
main = doRender

doRender :: IO ()
doRender = do
    putStrLn "Reading the starmap..."
    starmap <- readMapFromFile "PPM"
    case starmap of
        Right stars -> do
            stars `deepseq` putStrLn "Starmap read."
            putStrLn "Building the k-d star tree..."
            let startree = buildStarTree stars
            startree `deepseq` putStrLn "Star tree built."
            putStrLn "Rendering..."
            img <- computeP $ render myScene startree
            img `deepseq` putStrLn "Rendering completed."
            putStrLn "Saving to out.png..."
            doesFileExist "out.png" >>= (`when` removeFile "out.png")
            _ <- save PNG "out.png" ((convert img) :: RGB)
            putStrLn "Everything done. Thank you!"
            return ()
        _ -> putStrLn "Couldn't load the starmap."
