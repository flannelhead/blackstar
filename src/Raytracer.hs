{-# LANGUAGE BangPatterns #-}

module Raytracer where

import Vision.Primitive
import qualified Vision.Image as I
import Linear hiding (lookAt, mult)
import qualified Linear as L
import Control.Lens

data Scene = Scene { stepSize :: Double
                   , nSteps :: Int
                   , camera :: Camera }

data Camera = Camera { position :: V3 Double
                     , lookAt :: V3 Double
                     , upVec :: V3 Double
                     , fov :: Double
                     , resolution :: (Int, Int) }

-- Generate the sight rays ie. initial conditions for the integration
generateRay :: Scene -> Point -> (V3 Double, V3 Double)
{-# INLINE generateRay #-}
generateRay scn (Z :. y' :. x') = (vel, pos)
    where cam = camera scn
          pos = position cam
          xres = (fromIntegral . fst . resolution $ cam) :: Double
          yres = (fromIntegral . snd . resolution $ cam) :: Double
          matr = (L.lookAt pos (lookAt cam) (upVec cam)) ^. _m33
          vel  = normalize . (matr !*)
                 $ V3 (fov cam * ((fromIntegral x') / xres - 0.5))
                      (fov cam * ((fromIntegral y') / yres - 0.5) * yres/xres)
                      (-1)

render :: Scene -> I.RGB -> I.RGBDelayed
{-# INLINE render #-}
render scn tex = I.fromFunction (ix2 yres xres) (colorize tex . traceray scn)
    where cam = camera scn
          (xres, yres) = resolution cam

sqrnorm :: V3 Double -> Double
{-# INLINE sqrnorm #-}
sqrnorm (V3 !x !y !z) = x*x + y*y + z*z

traceray :: Scene -> Point -> (V3 Double, V3 Double)
{-# INLINE traceray #-}
traceray !scn !pt = last . take (nSteps scn)
                  . takeWhile (\(_, p) -> sqrnorm p > 1)
                  . iterate (rk4 (stepSize scn) (fgeodesic h2)) $ ray
                  where ray@(vel, pos) = generateRay scn pt
                        h2 = sqrnorm (pos `cross` vel)

colorize :: I.RGB -> (V3 Double, V3 Double) -> I.RGBPixel
{-# INLINE colorize #-}
colorize tex (!vel, !pos) = if sqrnorm pos < 2.25 then I.RGBPixel 0 0 0
                                                  else uvLookup tex vel

uv :: V3 Double -> (Double, Double)
{-# INLINE uv #-}
uv vec@(V3 !x !y !z) = (u, v) where
    u = 0.5 + (atan2 y x) / (2*pi)
    v = 0.5 - asin (z/r) / pi
    r = norm vec

uvLookup :: I.RGB -> V3 Double -> I.RGBPixel
{-# INLINE uvLookup #-}
uvLookup !tex !vel = tex `I.index`
                      (ix2 (floor $ v * (fromIntegral h))
                           (floor $ u * (fromIntegral w)))
    where (Z :. h :. w) = I.shape tex
          (u, v) = uv vel

rk4 :: Double -> ((V3 Double, V3 Double) -> (V3 Double, V3 Double))
       -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)
{-# INLINE rk4 #-}
rk4 !h !f !y = y `add`
    ((k1 `add` (k2 `mul` 2) `add` (k3 `mul` 2) `add` k4) `mul` (h/6))
    where k1 = f y
          k2 = f (y `add` (k1 `mul` (h/2)))
          k3 = f (y `add` (k2 `mul` (h/2)))
          k4 = f (y `add` (k3 `mul` h))

          add (!x, !z) (!u, !v) = (x ^+^ u, z ^+^ v)
          mul (!u, !v) !a = (u ^* a, v ^* a)

fgeodesic :: Double -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)
{-# INLINE fgeodesic #-}
fgeodesic h2 (!vel, !pos) = (-1.5*h2 / ((sqrnorm pos)**2.5) *^ pos, vel)
