module Util ( promptOverwriteFile, readSafe, normalizePath
            , timeAction, padZero ) where

import System.Directory
import System.IO
import qualified Data.ByteString.Lazy as B
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath
import Control.DeepSeq

readSafe :: FilePath -> IO (Either String B.ByteString)
readSafe path = do
    exists <- doesFileExist path
    if exists then Right <$> B.readFile path
              else return . Left $ "Error: file " ++ path
                  ++ " doesn't exist.\n"

promptOverwriteFile :: FilePath -> (FilePath -> IO ()) -> IO ()
promptOverwriteFile path doWrite = do
    doesExist <- doesFileExist path
    if doesExist then do
        putStr $ "Overwrite " ++ path ++ "? [y/N] "
        hFlush stdout
        answer <- getLine
        if answer == "y" || answer == "Y" then doWrite path
                                          else putStrLn "Nothing was written."
        else doWrite path

normalizePath :: FilePath -> IO FilePath
normalizePath path = (dropTrailingPathSeparator . normalise)
    <$> makeRelativeToCurrentDirectory path

timeAction :: NFData a => String -> a -> IO a
timeAction actionName value = do
    time1 <- (round <$> getPOSIXTime) :: IO Int
    let res = value
    time2 <- round <$> (res `deepseq` getPOSIXTime)
    let secs = time2 - time1
    putStrLn $ actionName ++ " completed in " ++ show (secs `div` 60)
        ++ " min " ++ show (secs `rem` 60) ++ " sec."
    return res

padZero :: Int -> Int -> String
padZero maxVal val = let
    nDigits x = (floor . logBase 10 $ (fromIntegral x :: Float)) + 1
    nZeros = nDigits maxVal - nDigits val
    zeros = replicate nZeros '0'
    in zeros ++ show val
