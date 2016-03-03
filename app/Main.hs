module Main where

import System.Environment (getArgs)
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
    args <- getArgs
    case args of
        []  -> doStart "default"
        [f] -> doStart f
        _   -> putStrLn "Mismatched command line arguments."

doStart :: String -> IO ()
doStart sceneName = do
    let filename = "scenes/" ++ sceneName ++ ".yaml"
    putStrLn $ "Reading " ++ filename ++ "..."
    cfg <- decodeFileEither filename
    case cfg of
        Right scene -> putStrLn "Config successfully read."
                           >> doRender scene sceneName
        Left  err   -> putStrLn $ prettyPrintParseException err

doRender :: Scene -> String -> IO ()
doRender scn sceneName = do
    putStrLn "Reading the starmap..."
    starmap <- readMapFromFile "PPM"
    case starmap of
        Right stars -> do
            putStrLn "Starmap read. Rendering..."
            let startree = buildStarTree stars
            img <- computeP $ render scn startree

            let outName = "output/" ++ sceneName ++ ".png"
            putStrLn $ "Rendering completed. Saving to " ++ outName ++ "..."
            doesDirectoryExist "output" >>= (`unless` createDirectory "output")
            overwriteImage outName img

            when (bloomStrength scn /= 0) $ do
                putStrLn "Applying bloom..."
                bloomed <- bloom (bloomStrength scn) img
                let bloomName = "output/" ++ sceneName ++ "-bloomed.png"
                putStrLn $ "Saving to " ++ bloomName ++ "..."
                overwriteImage bloomName bloomed

            putStrLn "Everything done. Thank you!"
        _ -> putStrLn "Couldn't load the starmap."

overwriteImage :: FilePath -> RGB -> IO ()
overwriteImage path img = do
    doesFileExist path >>= (`when` removeFile path)
    _ <- save PNG path img
    return ()
