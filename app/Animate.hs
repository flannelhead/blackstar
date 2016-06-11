{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Prelude hiding (writeFile)
import System.Directory
import System.FilePath
import Data.Yaml (decodeFileEither, prettyPrintParseException, encode)
import System.Console.CmdArgs
import Control.Monad (forM_)
import Data.ByteString.Lazy (fromStrict, writeFile)

import Animation
import Util

data Animate = Animate { inFile :: FilePath
                       , output :: FilePath
                       , force :: Bool }
                       deriving (Show, Data, Typeable)

argparser :: Animate
argparser = Animate { inFile = def
                            &= argPos 0
                            &= typ "INPUTFILE"
                    , output = ""
                            &= help "output directory"
                            &= typ "PATH"
                    , force  = def
                            &= help "overwrite images without asking" }
                    &= summary "Animation helper for Blackstar"

main :: IO ()
main = do
    cmdline <- cmdArgs argparser

    let inPath = inFile cmdline
    let basename = takeBaseName inPath
    inputExists <- doesFileExist inPath

    outPath <- normalizePath =<< case output cmdline of
                  "" -> getCurrentDirectory
                  x  -> return x
    createDirectoryIfMissing True outPath

    if inputExists then do
        config <- decodeFileEither inPath
        case config of
            Right cfg ->
                case validateKeyframes $ keyframes cfg of
                    Right () -> do
                        let nFr = nFrames cfg
                        forM_ (zip (generateFrames cfg) [(0 :: Int), 1 ..])
                            (\(frame, idx) -> do
                                let filename = outPath </> basename ++ "_" ++
                                        padZero (nFr - 1) idx <.> ".yaml"
                                let outBs = encode frame

                                let write = if force cmdline
                                    then writeFile
                                    else promptOverwriteFile
                                write filename $ fromStrict outBs
                            )
                    Left err -> putStrLn err
            Left err -> putStrLn $ "Error when decoding config:\n" ++
                                   prettyPrintParseException err
        else putStrLn "Couldn't open input file."
