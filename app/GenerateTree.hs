{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import qualified Data.ByteString.Lazy as B
import System.Console.CmdArgs

import Util
import StarMap

data GenerateTree = GenerateTree { infile :: FilePath
                                 , outfile :: FilePath
                                 , division :: Int }
                                 deriving (Show, Data, Typeable)

argparser :: GenerateTree
argparser = GenerateTree { infile = def
                             &= typ "INPUTFILE"
                             &= argPos 0
                         , outfile = def
                             &= typ "OUTPUTFILE"
                             &= argPos 1
                         , division = 20
                             &= help "division of the search space"
                             &= opt (20 :: Int)
                         } &= summary "GenerateTree utility v0.1"
                           &= program "generate-tree"

-- Generate and store the k-d star tree from a star catalog

main :: IO ()
main = do
    cmdline <- cmdArgs argparser
    outfile' <- normalizePath $ outfile cmdline
    infile' <- normalizePath $ infile cmdline
    eitherMap <- readMapFromFile infile'
    case eitherMap of
        Right stars -> do
            putStrLn "Generating the star tree..."
            tree <- timeAction "Building the tree"
                $ assembleStarGrid (division cmdline) stars
            let treeBl = B.fromStrict $ treeToByteString tree
            promptOverwriteFile outfile'
                (\filename -> B.writeFile filename treeBl)
            putStrLn $ "Tree saved to " ++ outfile' ++ "."
        Left  err   ->  putStrLn err
