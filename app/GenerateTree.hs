module Main where

import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)

import Util
import StarMap

-- Generate and store the k-d star tree from a star catalog

main :: IO ()
main = do
    args <- getArgs
    case args of
        [infile, outfile] -> do
            outfile' <- normalizePath outfile
            infile' <- normalizePath infile
            eitherMap <- readMapFromFile infile'
            case eitherMap of
                Right stars -> do
                    putStrLn "Generating the star tree..."
                    tree <- timeAction "Building the tree"
                        $ assembleStarGrid 10 stars
                    let treeBl = B.fromStrict $ treeToByteString tree
                    promptOverwriteFile outfile'
                        (\filename -> B.writeFile filename treeBl)
                    putStrLn $ "Tree saved to " ++ outfile' ++ "."
                Left  err   ->  putStrLn err
        _ -> putStrLn "Usage: generate-tree <INFILE> <OUTFILE>"
