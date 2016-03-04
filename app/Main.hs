module Main where

import System.Environment (getArgs)
import System.Directory
import Control.Monad
import Vision.Image
import Vision.Image.Storage.DevIL
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Raytracer
import StarMap
import Color
import ConfigFile

main :: IO ()
main = do
    args <- getArgs
    let filteredArgs = filter (`notElem` ["-p", "--preview"]) args
    let preview = length args /= length filteredArgs
    case filteredArgs of
        []  -> doStart "default" preview
        [f] -> doStart f preview
        _   -> putStrLn "USAGE: blackstar [-p|--preview] [scenename]"

doStart :: String -> Bool -> IO ()
doStart sceneName preview = do
    let filename = "scenes/" ++ sceneName ++ ".yaml"
    putStrLn $ "Reading " ++ filename ++ "..."
    cfg <- decodeFileEither filename
    case cfg of
        Right scene -> putStrLn "Scene successfully read."
                           >> doRender (prepareScene scene preview) sceneName
        Left  err   -> putStrLn $ prettyPrintParseException err

prepareScene :: Scene -> Bool -> Scene
prepareScene scn preview = let
    cam = camera scn
    (w, h) = resolution cam
    newRes = if w >= h then (300, 300 * h `div` w) else (300 * w `div` h, 300)
    in if preview then scn { camera = cam { resolution = newRes } } else scn

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
                Left  err'  -> do
                    putStrLn $ err'
                    return Nothing

doRender :: Scene -> String -> IO ()
doRender scn sceneName = do
    putStrLn "Reading the star tree..."
    mStarTree <- readStarTree
    case mStarTree of
        Just startree -> do
            putStrLn "Star tree read. Rendering..."
            time1 <- (round <$> getPOSIXTime) :: IO Int
            img <- computeP $ render scn startree
            time2 <- round <$> getPOSIXTime
            let secs = time2 - time1
            putStrLn $ "Rendering completed in " ++ show (secs `div` 60)
                ++ " min " ++ show (secs `rem` 60) ++ " sec."

            let outName = "output/" ++ sceneName ++ ".png"
            putStrLn $ "Saving to " ++ outName ++ "..."
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
