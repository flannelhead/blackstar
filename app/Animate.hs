{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import System.Directory
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import System.Console.CmdArgs


data Animate = Animate { nFrames :: Int
                       , startFile :: FilePath
                       , endFile :: FilePath
                       , outDir :: FilePath }
                       deriving (Show, Data, Typeable)

main :: IO ()
main = putStrLn "Hello World"
