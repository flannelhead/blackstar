{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import System.Directory
import System.IO
import qualified Data.ByteString.Lazy as B
import Control.Monad (unless, when)
import qualified Data.Array.Repa as R
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Console.CmdArgs
import System.FilePath

import Raytracer
import StarMap
import Color
import ConfigFile
import ImageFilters

data Blackstar = Blackstar { preview :: Bool
                           , output :: String
                           , force :: Bool
                           , inputfile :: String }
                           deriving (Show, Data, Typeable)

argparser :: Blackstar
argparser = Blackstar { preview = def
                          &= help "preview render (small size)"
                      , output = ""
                          &= help "output directory"
                          &= typ "PATH"
                      , force = def
                          &= help "overwrite images without asking"
                      , inputfile = def
                          &= argPos 0
                          &= typ "INPUTFILE"
                      } &= summary "Blackstar v0.1"

main :: IO ()
main = do
    cmdline <- cmdArgs argparser
    doStart cmdline

normalizePath :: FilePath -> IO FilePath
normalizePath path = (dropTrailingPathSeparator . normalise)
    <$> makeRelativeToCurrentDirectory path

doStart :: Blackstar -> IO ()
doStart cmdline = do
    filename <- normalizePath $ inputfile cmdline
    let pvw = preview cmdline
    let sceneName = takeBaseName filename
    when (output cmdline /= "")
        $ createDirectoryIfMissing True (output cmdline)
    outdir <- normalizePath =<< case output cmdline of
                  "" -> getCurrentDirectory
                  x  -> return x
    putStrLn $ "Reading " ++ filename ++ "..."
    cfg <- decodeFileEither filename
    let sceneName' = if pvw then sceneName ++ "-preview" else sceneName
    case cfg of
        Right scene -> putStrLn "Scene successfully read."
                         >> doRender cmdline (prepareScene scene pvw) sceneName'
                              outdir
        Left  err   -> putStrLn $ prettyPrintParseException err

prepareScene :: Scene -> Bool -> Scene
prepareScene scn doPreview = let
    cam = camera scn
    (w, h) = resolution cam
    res = 300
    newRes = if w >= h then (res, res * h `div` w) else (res * w `div` h, res)
    in if doPreview then scn { camera = cam { resolution = newRes } } else scn

readStarTree :: IO (Maybe StoredStarTree)
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
timeAction actionName action = do
    time1 <- (round <$> getPOSIXTime) :: IO Int
    res <- action
    time2 <- round <$> getPOSIXTime
    let secs = time2 - time1
    putStrLn $ actionName ++ " completed in " ++ show (secs `div` 60)
        ++ " min " ++ show (secs `rem` 60) ++ " sec."
    return res

doRender :: Blackstar -> Scene -> String -> String -> IO ()
doRender cmdline scn sceneName outdir = do
    let doWrite = if force cmdline then B.writeFile else promptOverwriteFile
    putStrLn "Reading the star tree..."
    mStarTree <- readStarTree
    case mStarTree of
        Just startree -> do
            putStrLn "Star tree read. Rendering..."
            img <- timeAction "Rendering"
                $ R.computeUnboxedP (render scn $ convertTree startree)

            outName <- normalizePath $ outdir ++ "/" ++ sceneName ++ ".png"
            putStrLn $ "Saving to " ++ outName ++ "..."
            doesDirectoryExist "output" >>= (`unless` createDirectory "output")
            doWrite outName $ pngByteString img

            when (bloomStrength scn /= 0) $ do
                putStrLn "Applying bloom..."
                bloomed <- timeAction "Bloom"
                    $ bloom (bloomStrength scn) (bloomDivider scn) img
                bloomName <- normalizePath
                    $ outdir ++ "/" ++ sceneName ++ "-bloomed.png"
                putStrLn $ "Saving to " ++ bloomName ++ "..."
                doWrite bloomName $ pngByteString bloomed

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
