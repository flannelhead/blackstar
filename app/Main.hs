module Main where

import Geometry
import Criterion.Main
import System.Random
import Control.Monad

main :: IO ()
-- main = benchmarkGeodesics
main = profileGeodesics

benchmarkGeodesics :: IO ()
benchmarkGeodesics = defaultMain [ bench "geodesic equations" $ whnf
                                   (fgeodesic schwarz ischwarz (FV 1 1 1 1))
                                   (FV 0 2 (pi/2) 0) ]

profileGeodesics :: IO ()
profileGeodesics = replicateM_ 100000 $ do
    r <- getStdRandom (randomR (2 :: Double, 3))
    let x = fgeodesic schwarz ischwarz (FV 1 1 r 1) (FV 0 r (pi/2) 0)
    return $! x
