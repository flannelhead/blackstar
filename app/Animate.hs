{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import System.Directory
import System.FilePath
import Data.Yaml (decodeFileEither, prettyPrintParseException, encodeFile)
import System.Console.CmdArgs
import Control.Monad (forM_)

import Animation
import Util

data Animate = Animate { inFile :: FilePath
                       , outDir :: FilePath }
                       deriving (Show, Data, Typeable)

argparser :: Animate
argparser = Animate { inFile = def
                            &= argPos 0
                            &= typ "INPUTFILE"
                    , outDir = def
                            &= argPos 1
                            &= typ "OUTDIR" }

main :: IO ()
main = do
    cmdline <- cmdArgs argparser

    let inPath = inFile cmdline
    let basename = takeBaseName inPath
    inputExists <- doesFileExist inPath

    outPath <- normalizePath $ outDir cmdline
    createDirectoryIfMissing True outPath

    if inputExists then do
        config <- decodeFileEither inPath
        case config of
            Right cfg -> do
                let nFr = nFrames cfg
                forM_ (zip (generateFrames cfg) [(0 :: Int), 1 ..])
                    (\(frame, idx) -> do
                        let filename = outPath </> basename ++ "_" ++
                                padZero (nFr - 1) idx <.> ".yaml"
                        encodeFile filename frame
                    )
            Left err -> putStrLn $ "Error when decoding config:\n" ++
                                   prettyPrintParseException err
        else putStrLn "Couldn't open input file."
