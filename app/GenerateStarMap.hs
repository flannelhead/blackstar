{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import qualified Data.ByteString.Lazy as B
import System.Console.CmdArgs

import Util
import StarMap

data GenerateStarMap = GenerateStarMap { infile :: FilePath
                                    , outfile :: FilePath
                                    , division :: Int }
                                    deriving (Show, Data, Typeable)

argparser :: GenerateStarMap
argparser = GenerateStarMap { infile = def
                             &= typ "INPUTFILE"
                             &= argPos 0
                         , outfile = def
                             &= typ "OUTPUTFILE"
                             &= argPos 1
                         , division = 20
                             &= help "division of the search space"
                             &= opt (20 :: Int)
                         } &= summary "GenerateStarMap utility v0.1"
                           &= program "generate-starmap"

-- Generate and store the star lookup map from a star catalog

main :: IO ()
main = do
    cmdline <- cmdArgs argparser
    outfile' <- normalizePath $ outfile cmdline
    infile' <- normalizePath $ infile cmdline
    eitherMap <- readMapFromFile infile'
    case eitherMap of
        Right stars -> do
            putStrLn "Generating the star map..."
            starmap <- timeAction "Building the map"
                $ assembleStarGrid (division cmdline) stars
            let starmapBl = B.fromStrict $ gridToByteString starmap
            promptOverwriteFile outfile'
                (\filename -> B.writeFile filename starmapBl)
            putStrLn $ "Map saved to " ++ outfile' ++ "."
        Left  err   ->  putStrLn err
