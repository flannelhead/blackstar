{-# LANGUAGE BangPatterns #-}

module Raytracer where

import Geometry
import qualified Data.Array.Repa as R
import Data.Array.Repa hiding (map, zipWith)
import Control.Monad

data Scene = Scene { stepSize :: Double
                   , nSteps :: Int
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
                   [ (fov cam * x'/xres - 0.5,
                     (fov cam * y'/yres - 0.5) * yres/xres, 1)
                     | x' <- [0..xres-1], y' <- [0..yres-1] ]
          rotate (u, v, w) = map (\(a, b, c) -> a*u + b*v + c*w)
              $ zip3 vleft vup vfwd
          norm [x, y, z] = sqrt (x*x + y*y + z*z)
          normalize v@[x, y, z] = let n = norm v in [x/n, y/n, z/n]
          cross [x, y, z] [u, v, w] = [y*w - z*v, u*z - x*w, x*v - u*y]
          fourPos [x, y, z] = (0, x, y, z)
          fourVel [u, v, w] = (1, u, v, w)

raytrace :: Scene -> Array D DIM1 (FourVector, FourVector)
raytrace scn = R.map trace . fromListUnboxed sh . generateRays $ cam
    where cam = camera scn
          (xres, yres) = resolution cam
          sh = Z :. xres * yres
          trace = last . take (nSteps scn)
                  . iterate (rk4 (stepSize scn) (fgeodesic scn))

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
