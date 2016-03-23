module Main where

import System.Environment (getArgs)
import System.Directory
import System.IO
import qualified Data.ByteString.Lazy as B
import Control.Monad
import qualified Data.Array.Repa as R
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Raytracer
import StarMap
import Color
import ConfigFile
import ImageFilters

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
    let sceneName' = if preview then sceneName ++ "-preview" else sceneName
    case cfg of
        Right scene -> putStrLn "Scene successfully read."
                           >> doRender (prepareScene scene preview) sceneName'
        Left  err   -> putStrLn $ prettyPrintParseException err

prepareScene :: Scene -> Bool -> Scene
prepareScene scn preview = let
    cam = camera scn
    (w, h) = resolution cam
    res = 300
    newRes = if w >= h then (res, res * h `div` w) else (res * w `div` h, res)
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
                    promptOverwriteFile treePath . B.fromStrict
                        $ treeToByteString tree'
                    putStrLn $ "Tree saved to " ++ treePath ++ "."
                    return $ Just tree'
                Left  err'  -> do
                    putStrLn err'
                    return Nothing

timeAction :: String -> IO a -> IO a
timeAction name action = do
    time1 <- (round <$> getPOSIXTime) :: IO Int
    res <- action
    time2 <- round <$> getPOSIXTime
    let secs = time2 - time1
    putStrLn $ name ++ " completed in " ++ show (secs `div` 60)
        ++ " min " ++ show (secs `rem` 60) ++ " sec."
    return res

doRender :: Scene -> String -> IO ()
doRender scn sceneName = do
    putStrLn "Reading the star tree..."
    mStarTree <- readStarTree
    case mStarTree of
        Just startree -> do
            putStrLn "Star tree read. Rendering..."
            img <- timeAction "Rendering"
                $ R.computeUnboxedP (render scn startree)

            let outName = "output/" ++ sceneName ++ ".png"
            putStrLn $ "Saving to " ++ outName ++ "..."
            doesDirectoryExist "output" >>= (`unless` createDirectory "output")
            promptOverwriteFile outName $ pngByteString img

            when (bloomStrength scn /= 0) $ do
                putStrLn "Applying bloom..."
                bloomed <- timeAction "Bloom" $ bloom (bloomStrength scn) img
                let bloomName = "output/" ++ sceneName ++ "-bloomed.png"
                putStrLn $ "Saving to " ++ bloomName ++ "..."
                promptOverwriteFile bloomName $ pngByteString bloomed

            putStrLn "Everything done. Thank you!"
        _ -> putStrLn "Couldn't load the star tree."

promptOverwriteFile :: FilePath -> B.ByteString -> IO ()
promptOverwriteFile path bs = do
    doesExist <- doesFileExist path
    if doesExist then do
        putStr $ "Overwrite " ++ path ++ "? [y/N] "
        hFlush stdout
        answer <- getLine
        if answer == "y" || answer == "Y" then B.writeFile path bs
                                          else putStrLn "Nothing was written."
                 else B.writeFile path bs
