{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Directory
import Control.Monad
import Vision.Primitive
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
                , starIntensity = 0.9
                , starSaturation = 0.8
                , renderDisk = True
                , diskOpacity = 0.8
                , diskIntensity = 1
                , diskSaturation = 1
                , diskInner = 3
                , diskOuter = 10 }

myCamera :: Camera
myCamera = Camera { position = V3 0 1 (-20)
                  , lookAt = V3 0 0 0
                  , upVec = V3 0.2 1 0
                  , fov = 1.5
                  , resolution = (1280, 720) }

main :: IO ()
main = testGaussian

doRender :: IO ()
doRender = do
    putStrLn "Reading the starmap..."
    starmap <- readMapFromFile "PPM"
    case starmap of
        Right stars -> do
            putStrLn "Starmap read. Rendering..."
            let startree = buildStarTree stars
            img <- computeP $ render myScene startree
            putStrLn "Applying Gaussian blur..."
            let (w, _) = resolution myCamera
            final <- gaussianBlur (w `div` 20) img
            putStrLn "Rendering completed. Saving to out.png..."
            doesFileExist "out.png" >>= (`when` removeFile "out.png")
            _ <- save PNG "out.png" final
            putStrLn "Everything done. Thank you!"
            return ()
        _ -> putStrLn "Couldn't load the starmap."

testGaussian :: IO ()
testGaussian = do
    let (w, h) = resolution myCamera
    putStrLn "Forming test image..."
    img <- computeP $ (fromFunction (ix2 h w)
        (\(Z :. y :. x) -> if y > x then RGBPixel 0 0 0
                                    else RGBPixel 255 255 255) :: RGB)
    putStrLn "Blurring it..."
    blurred <- gaussianBlur (w `div` 20) img
    doesFileExist "blurred.png" >>= (`when` removeFile "blurred.png")
    _ <- save PNG "blurred.png" blurred
    putStrLn "Done."
