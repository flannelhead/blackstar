module Util ( promptOverwriteFile, readSafe, normalizePath
            , timeAction ) where

import System.Directory
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath

readSafe :: FilePath -> IO (Either String B.ByteString)
readSafe path = do
    exists <- doesFileExist path
    if exists then Right <$> B.readFile path
              else return . Left $ "Error: file " ++ path
                  ++ " doesn't exist.\n"

promptOverwriteFile :: FilePath -> BL.ByteString -> IO ()
promptOverwriteFile path bs = do
    doesExist <- doesFileExist path
    if doesExist then do
        putStr $ "Overwrite " ++ path ++ "? [y/N] "
        hFlush stdout
        answer <- getLine
        if answer == "y" || answer == "Y" then BL.writeFile path bs
                                          else putStrLn "Nothing was written."
        else BL.writeFile path bs

normalizePath :: FilePath -> IO FilePath
normalizePath path = (dropTrailingPathSeparator . normalise)
    <$> makeRelativeToCurrentDirectory path

timeAction :: String -> IO a -> IO a
timeAction actionName action = do
    time1 <- (round <$> getPOSIXTime) :: IO Int
    res <- action
    time2 <- round <$> getPOSIXTime
    let secs = time2 - time1
    putStrLn $ actionName ++ " completed in " ++ show (secs `div` 60)
        ++ " min " ++ show (secs `rem` 60) ++ " sec."
    return res
