{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module Raytracer (render) where

import qualified Data.Array.Repa as R
import Data.Array.Repa.Index
import Linear hiding (lookAt, mult, trace)
import qualified Linear as L
import Control.Lens

import StarMap
import Color
import ConfigFile
import ImageFilters

data Layer = Layer RGBA | Bottom RGBA | None

-- Generate the sight rays ie. initial conditions for the integration
generateRay :: Config -> DIM2 -> (V3 Double, V3 Double)
generateRay cfg (Z :. y' :. x') = (vel, pos)
    where cam = camera cfg
          pos = position cam
          scn = scene cfg
          w = fromIntegral . fst $ resolution scn
          h = fromIntegral . snd $ resolution scn
          matr = L.lookAt pos (lookAt cam) (upVec cam) ^. _m33
          vel  = normalize . (transpose matr !*)
                 $ V3 (fov cam * (fromIntegral x' / w - 0.5))
                      (fov cam * (0.5 - fromIntegral y' / h) * h/w)
                      (-1)

render :: Config -> StarTree -> IO RGBImage
render cfg startree = let
    scn = scene cfg
    cam = camera cfg
    (w, h) = resolution scn
    res@(w', h') = if supersampling scn then (2*w, 2*h) else (w, h)
    scn' = scn { safeDistance =
                   max (50^(2 :: Int)) (2 * sqrnorm (position cam))
               , diskInner = diskInner scn ^ (2 :: Int)
               , diskOuter = diskOuter scn ^ (2 :: Int)
               , resolution = res }
    cfg' = cfg { scene = scn' }
    diskRGB = hsvToRGB $ diskColor scn
    img = R.fromFunction (ix2 h' w') (traceRay cfg' diskRGB startree)
    in R.computeUnboxedP $ if supersampling scn then supersample img else img


traceRay :: Config -> RGB -> StarTree -> DIM2 -> (Double, Double, Double)
traceRay cfg diskRGB startree pt = let
        ray@(vel, pos) = generateRay cfg pt
        h2 = sqrnorm $ pos `cross` vel
        scn = scene cfg
    in toTuple . dropAlpha . colorize scn diskRGB startree h2 $ ray

colorize :: Scene -> RGB -> StarTree -> Double -> (V3 Double, V3 Double) -> RGBA
colorize scn diskRGB startree h2 crd = let
    colorize' rgba crd' = let
        newCrd = rk4 (stepSize scn) h2 crd'
        in case findColor scn diskRGB startree crd' newCrd of
            Layer rgba' -> colorize' (blend rgba rgba') newCrd
            Bottom rgba' -> blend rgba rgba'
            None -> colorize' rgba newCrd
    in colorize' (RGBA 0 0 0 0) crd

findColor :: Scene -> RGB -> StarTree -> (V3 Double, V3 Double)
             -> (V3 Double, V3 Double) -> Layer
{-# INLINE findColor #-}
findColor scn diskRGB startree (vel, pos@(V3 _ y _)) (_, newPos@(V3 _ y' _))
    | r2 < 1 = Bottom (RGBA 0 0 0 1)  -- already passed the event horizon
    | r2 > safeDistance scn = Bottom  -- sufficiently far away
        $ starLookup startree (starIntensity scn) (starSaturation scn) vel
    | diskOpacity scn /= 0 && signum y' /= signum y
        && r2ave > diskInner scn && r2ave < diskOuter scn
        = Layer $ diskColor' scn diskRGB (sqrt r2ave)
    | otherwise = None
    where r2 = sqrnorm pos
          r2' = sqrnorm newPos
          r2ave = (y'*r2 - y*r2') / (y' - y)

diskColor' :: Scene -> RGB -> Double -> RGBA
{-# INLINE diskColor' #-}
diskColor' scn diskRGB !r = let
        rInner = sqrt (diskInner scn)
        rOuter = sqrt (diskOuter scn)
        alpha = sin (pi * ((rOuter-r) / (rOuter-rInner))^(2 :: Int))
    in addAlpha diskRGB (alpha * diskOpacity scn)

rk4 :: Double -> Double -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)
{-# INLINE rk4 #-}
rk4 !h !h2 y = y `add`
    ((k1 `add` (k2 `mul` 2) `add` (k3 `mul` 2) `add` k4) `mul` (h/6))
    where k1 = f y
          k2 = f (y `add` (k1 `mul` (h/2)))
          k3 = f (y `add` (k2 `mul` (h/2)))
          k4 = f (y `add` (k3 `mul` h))

          {-# INLINE mul #-}
          mul (u, v) !a = (u ^* a, v ^* a)
          {-# INLINE add #-}
          add (x, z) (u, v) = (x ^+^ u, z ^+^ v)
          {-# INLINE f #-}
          f (vel, pos) = (-1.5*h2 / (norm pos ^ (5 :: Int)) *^ pos, vel)
