{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import System.Directory
import qualified Data.ByteString.Lazy as BL
import Control.Monad (when, forM_)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import System.Console.CmdArgs
import System.FilePath (takeBaseName, takeExtension, (</>), (<.>))

import Raytracer
import StarMap
import Color
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
                . filter (\f -> takeExtension f == ".yaml")
                <$> getDirectoryContents filename

            forM_ inputFiles $ handleScene cmdline tree outdir
        else handleScene cmdline tree outdir filename

handleScene :: Blackstar -> StarTree -> String -> String -> IO ()
handleScene cmdline tree outdir filename = do
    let pvw = preview cmdline
    let sceneName = takeBaseName filename
    putStrLn $ "\nReading " ++ filename ++ "..."
    cfg <- decodeFileEither filename
    let sceneName' = if pvw then sceneName ++ "-preview" else sceneName
    case cfg of
        Right scene -> putStrLn "Scene successfully read."
                         >> doRender cmdline (prepareScene scene pvw) tree
                              sceneName' outdir
        Left  err   -> putStrLn $ prettyPrintParseException err

prepareScene :: Scene -> Bool -> Scene
prepareScene scn doPreview = let
    (w, h) = resolution scn
    res = 300
    newRes = if w >= h then (res, res * h `div` w) else (res * w `div` h, res)
    in if doPreview then scn { resolution = newRes } else scn

doRender :: Blackstar -> Scene -> StarTree -> String -> String -> IO ()
doRender cmdline scn tree sceneName outdir = do
    let doWrite = if force cmdline then BL.writeFile else promptOverwriteFile
    putStrLn $ "Rendering " ++ sceneName ++ "..."
    img <- timeAction "Rendering" $ render scn tree

    let outName = outdir </> sceneName <.> ".png"
    putStrLn $ "Saving to " ++ outName ++ "..."
    doWrite outName $ pngByteString img

    when (bloomStrength scn /= 0) $ do
        putStrLn "Applying bloom..."
        bloomed <- timeAction "Bloom"
            $ bloom (bloomStrength scn) (bloomDivider scn) img
        let bloomName = outdir </> (sceneName ++ "-bloomed") <.> ".png"
        putStrLn $ "Saving to " ++ bloomName ++ "..."
        doWrite bloomName $ pngByteString bloomed

    putStrLn "Everything done. Thank you!"
