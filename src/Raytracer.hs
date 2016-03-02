{-# LANGUAGE BangPatterns #-}

module Raytracer (
    render,
    Scene(Scene, stepSize, nSteps, camera),
    Camera(Camera, position, lookAt, upVec, fov, resolution)
    ) where

import Vision.Primitive
import qualified Vision.Image as I
import Linear hiding (lookAt, mult, trace)
import qualified Linear as L
import Control.Lens

import StarMap
import Color

data Scene = Scene { stepSize :: Double
                   , nSteps :: Int
                   , camera :: Camera }

data Camera = Camera { position :: V3 Double
                     , lookAt :: V3 Double
                     , upVec :: V3 Double
                     , fov :: Double
                     , resolution :: (Int, Int) }

data Layer = Layer !Rgba | Bottom !Rgba | None

-- Generate the sight rays ie. initial conditions for the integration
generateRay :: Scene -> Point -> (V3 Double, V3 Double)
generateRay !scn !(Z :. y' :. x') = (vel, pos)
    where cam = camera scn
          pos = position cam
          xres = fromIntegral . fst $ resolution cam
          yres = fromIntegral . snd $ resolution cam
          matr = L.lookAt pos (lookAt cam) (upVec cam) ^. _m33
          vel  = normalize . (transpose matr !*)
                 $ V3 (fov cam * ((fromIntegral x') / xres - 0.5))
                      (fov cam * ((fromIntegral y') / yres - 0.5) * yres/xres)
                      (-1)

render :: Scene -> StarTree -> I.RGBDelayed
render !scn !startree = I.fromFunction (ix2 yres xres) (traceray scn startree)
    where cam = camera scn
          (xres, yres) = resolution cam

traceray :: Scene -> StarTree -> Point -> I.RGBPixel
traceray !scn !startree !pt = toRGBPixel
    . colorize startree (rk4 (stepSize scn) (fgeodesic h2)) $ ray
    where ray@(vel, pos) = generateRay scn pt
          h2 = sqrnorm $ pos `cross` vel

colorize :: StarTree -> ((V3 Double, V3 Double) -> (V3 Double, V3 Double))
            -> (V3 Double, V3 Double) -> Rgba
colorize !startree !next !crd = let newCrd = next crd in
    case findColor startree crd newCrd of
        Layer rgba -> blend rgba $ colorize startree next newCrd
        Bottom rgba -> rgba
        None -> colorize startree next newCrd

findColor :: StarTree -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)
             -> Layer
findColor !startree (!vel, pos@(V3 _ !y _)) (_, newPos@(V3 _ !y' _))
    | r2 < 1 = Bottom $ Rgba 0 0 0 1  -- already entered the photon sphere
    | r2 > 30**2 = Bottom $ starLookup startree vel  -- sufficiently far away
    | (signum y' /= signum y) && r2ave > 6.72 && r2ave < 196 =
        Layer $ Rgba 0 0 255 0.5
    | otherwise = None
    where r2 = sqrnorm pos
          r2' = sqrnorm newPos
          r2ave = (y'*r2 - y*r2') / (y' - y)

rk4 :: Double -> ((V3 Double, V3 Double) -> (V3 Double, V3 Double))
       -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)
{-# INLINE rk4 #-}
rk4 !h !f !y = y `add`
    ((k1 `add` (k2 `mul` 2) `add` (k3 `mul` 2) `add` k4) `mul` (h/6))
    where k1 = f y
          k2 = f (y `add` (k1 `mul` (h/2)))
          k3 = f (y `add` (k2 `mul` (h/2)))
          k4 = f (y `add` (k3 `mul` h))

          mul (!u, !v) !a = (u ^* a, v ^* a)
          add (!x, !z) (!u, !v) = (x ^+^ u, z ^+^ v)

fgeodesic :: Double -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)
{-# INLINE fgeodesic #-}
fgeodesic h2 (!vel, !pos) = (-1.5*h2 / ((sqrnorm pos)**2.5) *^ pos, vel)
