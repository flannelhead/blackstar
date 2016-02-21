{-# LANGUAGE BangPatterns #-}

module Raytracer where

import Geometry
import Control.Monad
import System.Random

data Scene = Scene { stepSize :: Double
                   , nSteps :: Int
                   , nRays :: Int
                   , toCartesian :: FourVector -> FourVector
                   , fromCartesian :: FourVector -> FourVector
                   , fgeodesic :: FourVector -> FourVector -> FourVector }

trace :: Scene -> IO ()
trace scn = replicateM_ (nRays scn) $ do
    r <- getStdRandom (randomR (10 :: Double, 20))
    let vel = (fromCartesian scn) . rayVelocity $ (1, 1, 0)
    let pos = (fromCartesian scn) $ (0, r, 0, 0)
    let x = last . take (nSteps scn)
            $ iterate (rk4 (stepSize scn) schwarzGeodesic) (vel, pos)
    return $! x

rk4 :: Double
       -> (FourVector -> FourVector -> FourVector)
       -> (FourVector, FourVector)
       -> (FourVector, FourVector)
{-# INLINE rk4 #-}
rk4 !h !f !y = y `add'`
    ((k1 `add'` (k2 `mult'` 2) `add'` (k3 `mult'` 2) `add'` k4) `mult'` (h/6))
    where k1 = f' y
          k2 = f' (y `add'` (k1 `mult'` (h/2)))
          k3 = f' (y `add'` (k2 `mult'` (h/2)))
          k4 = f' (y `add'` (k3 `mult'` h))

          f' (!v, !c) = (f v c, v)
          add' (!x, !z) (!u, !v) = (x `add` u, z `add` v)
          mult' (!u, !v) !a = (u `mult` a, v `mult` a)
