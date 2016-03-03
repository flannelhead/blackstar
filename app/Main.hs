module Main where

import System.Directory
import Control.Monad
import Vision.Image
import Vision.Image.Storage.DevIL
import Data.Yaml (decodeFileEither, prettyPrintParseException)

import Raytracer
import StarMap
import Color
import ConfigFile

main :: IO ()
main = do
    putStrLn "Reading default.yaml..."
    cfg <- decodeFileEither "scenes/default.yaml"
    case cfg of
        Right scene -> putStrLn "Config successfully read." >> doRender scene
        Left  err   -> putStrLn $ "Reading config failed: "
                         ++ prettyPrintParseException err

doRender :: Scene -> IO ()
doRender scn = do
    putStrLn "Reading the starmap..."
    starmap <- readMapFromFile "PPM"
    case starmap of
        Right stars -> do
            putStrLn "Starmap read. Rendering..."
            let startree = buildStarTree stars
            img <- computeP $ render scn startree
            putStrLn "Rendering completed. Saving to out.png..."
            overwriteImage "out.png" img
            putStrLn "Applying bloom..."
            final <- bloom (bloomStrength scn) img
            putStrLn "Saving to bloomed.pnd..."
            overwriteImage "bloomed.png" final
            putStrLn "Everything done. Thank you!"
            return ()
        _ -> putStrLn "Couldn't load the starmap."

overwriteImage :: FilePath -> RGB -> IO ()
overwriteImage path img = do
    doesFileExist path >>= (`when` removeFile path)
    _ <- save PNG path img
    return ()
