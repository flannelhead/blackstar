module Main where

import GR hiding (toList)
import Criterion.Main
import Data.Array.Repa hiding (map)

main :: IO ()
main = benchmarkChristoffels
-- main = printChristoffels

benchmarkChristoffels :: IO ()
benchmarkChristoffels = defaultMain [ bench "christoffels"
                                      $ whnf (christoffels schwarz ischwarz)
                                        (FVect (0 :: Double) 2 (pi/2) 0) ]

printChristoffels :: IO ()
printChristoffels = putStrLn $ unlines (map (show . toList) rows)
    where chris = christoffels schwarz ischwarz (FVect (0 :: Double) 2 (pi/2) 0)
          rows :: [Array U DIM1 Double]
          rows = [ computeS $ slice chris (Any :. (l :: Int) :. All)
                   | l <- [0..3] ]


