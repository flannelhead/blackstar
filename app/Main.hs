module Main where

import Geometry
import Raytracer
import Criterion.Main
import System.Random
import Control.Monad

h :: Double
h = 0.1
steps :: Int
steps = 250

main :: IO ()
-- main = benchmarkGeodesics
-- main = manyGeodesics
main = manyRays

benchmarkGeodesics :: IO ()
benchmarkGeodesics = defaultMain [ bench "ad geodesic equations" $ whnf
                                   (fgeodesic schwarz ischwarz (FV 1 1 1 1))
                                   (FV 0 2 (pi/2) 0) ]

manyGeodesics :: IO ()
manyGeodesics = replicateM_ 100000 $ do
    r <- getStdRandom (randomR (2 :: Double, 3))
    let x = fgeodesic schwarz ischwarz (FV 0 1 r 1) (FV 0 r (pi/2) 0)
    return $! x

manyRays :: IO ()
manyRays = replicateM_ 100 $ do
    r <- getStdRandom (randomR (10 :: Double, 20))
    let vel = cartesianToSchwarz . rayVelocity $ (1, 1, 0)
    let pos = cartesianToSchwarz $ FV 0 r 0 0
    let x = last . take steps $ iterate (rk4 h (fgeodesic schwarz ischwarz))
              (vel, pos)
    return $! x
