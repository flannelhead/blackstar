module Main where

import Geodesics hiding (toList)
import Criterion.Main
import Data.Array.Repa hiding (map)
import System.Random
import Control.Monad

main :: IO ()
main = benchmarkChristoffels
-- main = benchmarkChristoffels2
-- main = printChristoffels

benchmarkChristoffels :: IO ()
benchmarkChristoffels = defaultMain [ bench "christoffels"
                                      $ whnf (christoffels schwarz ischwarz)
                                        (FVect (0 :: Double) 2 (pi/2) 0) ]

benchmarkChristoffels2 :: IO ()
benchmarkChristoffels2 = replicateM_ 100000 $ do
    r <- getStdRandom (randomR (2 :: Double, 3))
    let chris = christoffels schwarz ischwarz (FVect 0 r (pi/2) 0)
    return $! chris

printChristoffels :: IO ()
printChristoffels = putStrLn $ unlines (map (show . toList) rows)
    where chris = christoffels schwarz ischwarz (FVect (0 :: Double) 2 (pi/2) 0)
          rows :: [Array U DIM1 Double]
          rows = [ computeS $ slice chris (Any :. (l :: Int) :. All)
                   | l <- [0..3] ]


