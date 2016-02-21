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
                   , fgeodesic :: FourVector -> FourVector -> FourVector
                   , camera :: Camera }

data Camera = Camera { position :: [Double]
                     , lookAt :: [Double]
                     , upVec :: [Double]
                     , fov :: Double
                     , resolution :: (Int, Int) }

-- Generate the sight rays ie. initial conditions for the integration
generateRays :: Camera -> [(FourVector, FourVector)]
generateRays cam = map (\v -> (fourVel v, fourPos . position $ cam)) vecs
    where vfwd = normalize (zipWith (-) (position cam) (lookAt cam))
          vleft = normalize (upVec cam `cross` vfwd)
          vup = vfwd `cross` vleft
          xres = (fromIntegral . fst . resolution $ cam) :: Double
          yres = (fromIntegral . snd . resolution $ cam) :: Double
          vecs = map (normalize . rotate)
                   [ (fov cam * x'/xres - 0.5, fov cam * y'*yres/xres - 0.5, 1)
                     | x' <- [0..xres-1], y' <- [0..yres-1] ]
          rotate (u, v, w) = map (\(a, b, c) -> a*u + b*v + c*w)
              $ zip3 vleft vup vfwd
          norm [x, y, z] = sqrt (x*x + y*y + z*z)
          normalize v@[x, y, z] = let n = norm v in [x/n, y/n, z/n]
          cross [x, y, z] [u, v, w] = [y*w - z*v, u*z - x*w, x*v - u*y]
          fourPos [x, y, z] = (0, x, y, z)
          fourVel [u, v, w] = (1, u, v, w)

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
