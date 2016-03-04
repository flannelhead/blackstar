{-# LANGUAGE BangPatterns #-}

module Raytracer (render) where

import qualified Data.Array.Repa as R
import Data.Array.Repa.Index
import Linear hiding (lookAt, mult, trace)
import qualified Linear as L
import Control.Lens

import StarMap
import Color
import ConfigFile

data Layer = Layer !RGBA | Bottom !RGBA | None

-- Generate the sight rays ie. initial conditions for the integration
generateRay :: Scene -> DIM2 -> (V3 Double, V3 Double)
generateRay !scn !(Z :. y' :. x') = (vel, pos)
    where cam = camera scn
          pos = position cam
          w = fromIntegral . fst $ resolution cam
          h = fromIntegral . snd $ resolution cam
          matr = L.lookAt pos (lookAt cam) (upVec cam) ^. _m33
          vel  = normalize . (transpose matr !*)
                 $ V3 (fov cam * ((fromIntegral x') / w - 0.5))
                      (fov cam * (0.5 - (fromIntegral y') / h) * h/w)
                      (-1)

render :: Scene -> StarTree -> RGBImageDelayed
render !scn !startree = R.fromFunction (ix2 h w) (traceRay scn' startree)
    where cam = camera scn
          (w, h) = resolution cam
          scn' = scn { safeDistance = max (50**2) (2 * (sqrnorm $ position cam))
                     , diskInner = (diskInner scn)**2
                     , diskOuter = (diskOuter scn)**2 }

traceRay :: Scene -> StarTree -> DIM2 -> RGB
traceRay !scn !startree !pt = let
        ray@(vel, pos) = generateRay scn pt
        h2 = sqrnorm $ pos `cross` vel
    in dropAlpha
       . colorize scn startree (rk4 (stepSize scn) (fgeodesic h2)) $ ray

colorize :: Scene -> StarTree
            -> ((V3 Double, V3 Double) -> (V3 Double, V3 Double))
            -> (V3 Double, V3 Double) -> RGBA
colorize !scn !startree !next !crd = let newCrd = next crd in
    case findColor scn startree crd newCrd of
        Layer rgba -> blend rgba $ colorize scn startree next newCrd
        Bottom rgba -> rgba
        None -> colorize scn startree next newCrd

findColor :: Scene -> StarTree -> (V3 Double, V3 Double)
             -> (V3 Double, V3 Double) -> Layer
findColor !scn !startree (!vel, pos@(V3 !x !y !z)) (_, newPos@(V3 !x' !y' !z'))
    | r2 < 1 = Bottom $ (0, 0, 0, 1)  -- already passed the event horizon
    | r2 > safeDistance scn = Bottom  -- sufficiently far away
        $ starLookup startree (starIntensity scn) (starSaturation scn) vel
    | diskOpacity scn /= 0 && (signum y' /= signum y)
        && r2ave > diskInner scn && r2ave < diskOuter scn
        = Layer $ diskColor scn (sqrt r2ave) phiave
    | otherwise = None
    where r2 = sqrnorm pos
          r2' = sqrnorm newPos
          r2ave = (y'*r2 - y*r2') / (y' - y)
          phiave = (y'*atan2 z x - y*atan2 z' x') / (y' - y)

diskColor :: Scene -> Double -> Double -> RGBA
diskColor !scn !r _ = let
        inner = sqrt (diskInner scn)
        dr = sqrt (diskOuter scn) - inner
        alpha = sin (pi*(1 - (r-inner)/dr)^(2 :: Int))
    in addAlpha (diskRGB scn) (alpha * diskOpacity scn)

rk4 :: Double -> ((V3 Double, V3 Double) -> (V3 Double, V3 Double))
       -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)
rk4 !h !f !y = y `add`
    ((k1 `add` (k2 `mul` 2) `add` (k3 `mul` 2) `add` k4) `mul` (h/6))
    where k1 = f y
          k2 = f (y `add` (k1 `mul` (h/2)))
          k3 = f (y `add` (k2 `mul` (h/2)))
          k4 = f (y `add` (k3 `mul` h))

          mul (!u, !v) !a = (u ^* a, v ^* a)
          add (!x, !z) (!u, !v) = (x ^+^ u, z ^+^ v)

fgeodesic :: Double -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)
fgeodesic h2 (!vel, !pos) = (-1.5*h2 / ((sqrnorm pos)**2.5) *^ pos, vel)
