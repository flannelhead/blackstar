{-# LANGUAGE BangPatterns #-}

module Raytracer where

import Geometry
import Vision.Primitive
import qualified Vision.Image as I

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
generateRay :: Scene -> Point -> (FourVector, FourVector)
generateRay scn (Z :. y' :. x') = (posEnd `add` (pos' `mult` (-1)), posEnd)
    where cam = camera scn
          pos = position cam
          pos' = fromCartesian scn $ fourPos pos
          -- Some really sketchy linear algebra stuff
          -- Rotation matrix for the view vector
          vfwd = normalize (zipWith (-) (lookAt cam) pos)
          vleft = normalize (upVec cam `cross` vfwd)
          vup = vfwd `cross` vleft
          rotate (u, v, w) = map (\(a, b, c) -> a*u + b*v + c*w)
              $ zip3 vleft vup vfwd
          xres = (fromIntegral . fst . resolution $ cam) :: Double
          yres = (fromIntegral . snd . resolution $ cam) :: Double
          posDiff = normalize . rotate
                    $ ( fov cam * ((fromIntegral x') / xres - 0.5)
                      , fov cam * ((fromIntegral y') / yres - 0.5) * yres/xres
                      , 1 )
          posEnd = fromCartesian scn (fourVel $ zipWith (+) pos posDiff)
          -- Basic linear algebra stuff
          norm [x, y, z] = sqrt (x*x + y*y + z*z)
          normalize v@[x, y, z] = let n = norm v in [x/n, y/n, z/n]
          cross [x, y, z] [u, v, w] = [y*w - z*v, u*z - x*w, x*v - u*y]
          fourPos [x, y, z] = (0, x, y, z)
          fourVel [u, v, w] = (1, u, v, w)

raytrace :: Scene -> I.HSVDelayed
raytrace scn = I.fromFunction sh (colorize . trace)
    where cam = camera scn
          (xres, yres) = resolution cam
          sh = ix2 yres xres
          trace = last . take (nSteps scn)
                  . iterate (rk4 (stepSize scn) (fgeodesic scn))
                  . generateRay scn

uvToHSV :: Double -> Double -> I.HSVPixel
uvToHSV _ v = I.HSVPixel (round (179 * 0.5 * (1 + sin(2*pi*v)))) 255 255

colorize :: (FourVector, FourVector) -> I.HSVPixel
colorize (v, _) = uvToHSV (0.5 + (atan2 y x) / (2*pi)) (0.5 - asin (z/r) / pi)
    where (_, x, y, z) = schwarzToCartesian v
          r = sqrt (x*x + y*y + z*z)

rk4 :: Double -> (FourVector -> FourVector -> FourVector)
       -> (FourVector, FourVector) -> (FourVector, FourVector)
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
