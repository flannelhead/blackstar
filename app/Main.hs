{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Directory
import Control.Monad
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
                  , resolution = (2732, 1536) }

main :: IO ()
main = doRender

doRender :: IO ()
doRender = do
    putStrLn "Reading the starmap..."
    starmap <- readMapFromFile "PPM"
    putStrLn "Starmap read."
    case starmap of
        Right stars -> do
            -- print $ sum (map snd stars) `div` length stars
            -- print $ minimum (map snd stars)
            putStrLn "Building the star k-d tree..."
            startree <- return $! buildStarTree stars
            putStrLn "Startree built."
            putStrLn "Rendering..."
            img <- computeP $ render myScene startree
            putStrLn "Rendering completed."
            putStrLn "Saving to out.png..."
            doesFileExist "out.png" >>= (`when` removeFile "out.png")
            _ <- save PNG "out.png" ((convert img) :: RGB)
            putStrLn "Everything done. Thank you!"
            return ()
        _ -> putStrLn "Couldn't load the texture"
