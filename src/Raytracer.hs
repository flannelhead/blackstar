{-# LANGUAGE BangPatterns #-}

module Raytracer
    ( render
    , Scene( Scene, stepSize, nSteps, camera, starIntensity, starSaturation
           , renderDisk, diskIntensity, diskSaturation, diskOpacity
           , diskInner, diskOuter)
    , Camera( Camera, position, lookAt, upVec, fov, resolution ) ) where

import Vision.Primitive
import qualified Vision.Image as I
import Linear hiding (lookAt, mult, trace)
import qualified Linear as L
import Control.Lens

import StarMap
import Color

data Scene = Scene { stepSize :: Double
                   , nSteps :: Int
                   , camera :: Camera
                   , starIntensity :: Double
                   , starSaturation :: Double
                   , renderDisk :: Bool
                   , diskIntensity :: Double
                   , diskSaturation :: Double
                   , diskOpacity :: Double
                   , diskInner :: Double
                   , diskOuter :: Double }

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
                      (fov cam * (0.5 - (fromIntegral y') / yres) * yres/xres)
                      (-1)

render :: Scene -> StarTree -> I.RGBDelayed
render !scn !startree = I.fromFunction (ix2 yres xres) (traceRay scn' startree)
    where cam = camera scn
          (xres, yres) = resolution cam
          scn' = scn { diskInner = (diskInner scn)**2
                     , diskOuter = (diskOuter scn)**2 }

traceRay :: Scene -> StarTree -> Point -> I.RGBPixel
traceRay !scn !startree !pt = toRGBPixel
    . colorize scn startree (rk4 (stepSize scn) (fgeodesic h2)) $ ray
    where ray@(vel, pos) = generateRay scn pt
          h2 = sqrnorm $ pos `cross` vel

colorize :: Scene -> StarTree
            -> ((V3 Double, V3 Double) -> (V3 Double, V3 Double))
            -> (V3 Double, V3 Double) -> Rgba
colorize !scn !startree !next !crd = let newCrd = next crd in
    case findColor scn startree crd newCrd of
        Layer rgba -> blend rgba $ colorize scn startree next newCrd
        Bottom rgba -> rgba
        None -> colorize scn startree next newCrd

findColor :: Scene -> StarTree -> (V3 Double, V3 Double)
             -> (V3 Double, V3 Double) -> Layer
findColor !scn !startree (!vel, pos@(V3 !x !y !z)) (_, newPos@(V3 !x' !y' !z'))
    | r2 < 1 = Bottom $ Rgba 0 0 0 1  -- already entered the photon sphere
    | r2 > 30**2 = Bottom  -- sufficiently far away so the curvature wont affect
        $ starLookup startree (starIntensity scn) (starSaturation scn) vel
    | renderDisk scn && (signum y' /= signum y)
        && r2ave > diskInner scn && r2ave < diskOuter scn
        = Layer $ diskColor scn (sqrt r2ave) phiave
    | otherwise = None
    where r2 = sqrnorm pos
          r2' = sqrnorm newPos
          r2ave = (y'*r2 - y*r2') / (y' - y)
          phiave = (y'*atan2 z x - y*atan2 z' x') / (y' - y)

diskColor :: Scene -> Double -> Double -> Rgba
diskColor !scn !r !phi = let
        inner = sqrt (diskInner scn)
        dr = sqrt (diskOuter scn) - inner
        alpha = 0.3 + 0.7 * (sin (12*pi*(r-inner)/dr + 3*pi/2) + 1) / 2
        pxl = I.HSVPixel 35 (floor $ 60 * (sin (12*phi) + 1) / 2)
                  (floor $ 170 + 55 * (sin (12*phi) + 1) / 2)
    in fromRGBPixelWithAlpha (I.convert pxl) (alpha * diskOpacity scn)

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
