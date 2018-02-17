{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import System.Directory
import Control.Monad (when, forM_)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import System.Console.CmdArgs
import System.FilePath (takeBaseName, takeExtension, (</>), (<.>))
import Data.List (sort)
import System.Console.ANSI (clearScreen, setCursorPosition)

import Raytracer
import StarMap
import ConfigFile
import ImageFilters
import Util

data Blackstar = Blackstar { preview :: Bool
                           , output :: String
                           , force :: Bool
                           , starmap :: String
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
                      , starmap = "stars.kdt"
                          &= help "path to starmap"
                          &= typ "PATH"
                      , inputfile = def
                          &= argPos 0
                          &= typ "INPUTFILE"
                      } &= summary "Blackstar v0.1"

main :: IO ()
main = do
    cmdline <- cmdArgs argparser
    etree <- readTreeFromFile $ starmap cmdline
    case etree of
        Right tree -> putStrLn "Starmap successfully read."
            >> doStart cmdline tree
        Left  err  -> putStrLn $ "Error decoding star tree: \n" ++ err

doStart :: Blackstar -> StarTree -> IO ()
doStart cmdline tree = do
    -- Resolve the output directory
    when (output cmdline /= "")
        $ createDirectoryIfMissing True (output cmdline)
    outdir <- normalizePath =<< case output cmdline of
                  "" -> getCurrentDirectory
                  x  -> return x
    createDirectoryIfMissing True outdir
    -- Resolve the input file or directory
    filename <- normalizePath $ inputfile cmdline
    isDir <- doesDirectoryExist filename
    if isDir then do
            putStrLn $ filename
                ++ " is a directory. Rendering all scenes inside it..."

            inputFiles <- map (filename </>)
                . sort . filter (\f -> takeExtension f == ".yaml")
                <$> getDirectoryContents filename

            forM_ (zip inputFiles [(1 :: Int)..]) $ \(scn, idx) -> do
                clearScreen
                setCursorPosition 0 0
                putStrLn $ "Batch mode progress: " ++ show idx ++ "/"
                    ++ show (length inputFiles)
                handleScene cmdline tree outdir scn
        else handleScene cmdline tree outdir filename

handleScene :: Blackstar -> StarTree -> String -> String -> IO ()
handleScene cmdline tree outdir filename = do
    let pvw = preview cmdline
    let sceneName = takeBaseName filename
    putStrLn $ "Reading " ++ filename ++ "..."
    cfg <- decodeFileEither filename
    let sceneName' = if pvw then "prev-" ++ sceneName else sceneName
    case cfg of
        Right config -> putStrLn "Scene successfully read."
                          >> doRender cmdline (prepareScene config pvw) tree
                               sceneName' outdir
        Left  err    -> putStrLn $ prettyPrintParseException err

prepareScene :: Config -> Bool -> Config
prepareScene cfg doPreview = let
    scn = scene cfg
    (w, h) = resolution scn
    res = 300
    newRes = if w >= h then (res, res * h `div` w) else (res * w `div` h, res)
    newScn = if doPreview then scn { resolution = newRes
                                   , supersampling = False
                                   , bloomStrength = 0 }
                          else scn
    in cfg { scene = newScn }

doRender :: Blackstar -> Config -> StarTree -> String -> String -> IO ()
doRender cmdline cfg tree sceneName outdir = do
    putStrLn $ "Rendering " ++ sceneName ++ "..."
    let scn = scene cfg
    img <- timeAction "Rendering" $ render cfg tree

    let outName = outdir </> sceneName <.> ".png"

    final <- if bloomStrength scn /= 0
        then do
            putStrLn "Applying bloom..."
            bloomed <- bloom (bloomStrength scn) (bloomDivider scn) img
            timeAction "Bloom" bloomed
        else return img

    putStrLn $ "Saving to " ++ outName ++ "..."
    if force cmdline
      then writeImg final outName
      else promptOverwriteFile outName (writeImg final)

    putStrLn "Everything done. Thank you!"
