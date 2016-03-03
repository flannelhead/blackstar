module Main where

import System.Directory
import Control.Monad
import Vision.Image hiding (map, gaussianBlur)
import Vision.Image.Storage.DevIL
import Linear hiding (lookAt)

import Raytracer
import StarMap
import Color

myScene :: Scene
myScene = Scene { stepSize = 0.15
                , nSteps = 250
                , camera = myCamera
                , bloomStrength = 0.7
                , starIntensity = 0.7
                , starSaturation = 0.7
                , renderDisk = True
                , diskOpacity = 0.95
                , diskInner = 3
                , diskOuter = 10 }

myCamera :: Camera
myCamera = Camera { position = V3 0 1 (-20)
                  , lookAt = V3 2 0 0
                  , upVec = V3 (-0.2) 1 0
                  , fov = 1.5
                  , resolution = (1280, 720) }

main :: IO ()
main = do
    putStrLn "Reading the starmap..."
    starmap <- readMapFromFile "PPM"
    case starmap of
        Right stars -> do
            putStrLn "Starmap read. Rendering..."
            let startree = buildStarTree stars
            img <- computeP $ render myScene startree
            putStrLn "Rendering completed. Saving to out.png..."
            doesFileExist "out.png" >>= (`when` removeFile "out.png")
            _ <- save PNG "out.png" img
            putStrLn "Applying bloom..."
            final <- bloom (bloomStrength myScene) img
            putStrLn "Saving to bloomed.pnd..."
            doesFileExist "bloomed.png" >>= (`when` removeFile "bloomed.png")
            _ <- save PNG "bloomed.png" final
            putStrLn "Everything done. Thank you!"
            return ()
        _ -> putStrLn "Couldn't load the starmap."
