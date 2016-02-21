module Main where

import Geometry
import Raytracer
import System.Random
import Control.Monad

h :: Double
h = 0.1

nSteps :: Int
nSteps = 250

nRays :: Int
nRays = 20000

manyRays :: IO ()
manyRays = replicateM_ nRays $ do
    r <- getStdRandom (randomR (10 :: Double, 20))
    let vel = cartesianToSchwarz . rayVelocity $ (1, 1, 0)
    let pos = cartesianToSchwarz $ FV 0 r 0 0
    let x = last . take nSteps $ iterate (rk4 h schwarzGeodesic)
              (vel, pos)
    return $! x

main :: IO ()
main = manyRays
