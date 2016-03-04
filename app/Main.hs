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

readStarTree :: IO (Maybe StarTree)
readStarTree = do
    let treePath = "stars.kdt"
    eitherTree <- readTreeFromFile treePath
    case eitherTree of
        Right tree -> return $ Just tree
        Left  err  -> do
            putStrLn $ err ++ "Generating the tree..."
            let mapPath = "PPM"
            eitherMap <- readMapFromFile mapPath
            case eitherMap of
                Right stars -> do
                    let tree' = buildStarTree stars
                    writeTreeToFile treePath tree'
                    putStrLn $ "Tree saved to " ++ treePath ++ "."
                    return $ Just tree'
                Left  err   -> do
                    putStrLn $ err
                    return Nothing

doRender :: Scene -> String -> IO ()
doRender scn sceneName = do
    putStrLn "Reading the star tree..."
    mStarTree <- readStarTree
    case mStarTree of
        Just startree -> do
            putStrLn "Star tree read. Rendering..."
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
        _ -> putStrLn "Couldn't load the star tree."

overwriteImage :: FilePath -> RGB -> IO ()
overwriteImage path img = do
    doesFileExist path >>= (`when` removeFile path)
    _ <- save PNG path img
    return ()
