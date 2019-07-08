{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Raytracer (blackstarProgram) where

import Control.Lens hiding (use)
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Colour.HSL as HSL
import Data.Array.Accelerate.Data.Colour.RGB
import Data.Array.Accelerate.Data.Colour.RGBA as RGBA
import Data.Array.Accelerate.Data.Colour.SRGB as SRGB
import Data.Array.Accelerate.Linear.Vector
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Matrix as M
import Data.Array.Accelerate.Linear.Projection as LP
import Data.Array.Accelerate.Linear.V2
import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate.Linear.V4
import Data.Array.Accelerate.IO.Codec.Picture
import Prelude()

import StarMap
import StarMapLookup
import ConfigFile

type Position = V3 Float
type Velocity = V3 Float
type Acceleration = V3 Float
type PhotonState = (Position, Velocity, Acceleration)
type RaytracerState = (PhotonState, RGBA Float)
type DiskParams = (RGBA Float, Float, Float, Float)

blend :: Exp (RGBA Float) -> Exp (RGBA Float) -> Exp (RGBA Float)
blend (unlift -> RGBA sr sg sb sa :: RGBA (Exp Float))
      (unlift -> RGBA dr dg db da :: RGBA (Exp Float)) = let
    g s d = s + d * (1 - sa)
    in lift $ RGBA (g sr dr) (g sg dg) (g sb db) (g sa da)

diskColor' :: Exp DiskParams -> Exp Float -> Exp (RGBA Float)
diskColor' (unlift -> (diskRGBA :: Exp (RGBA Float), rOuter, rInner, coef)) radius = let
    RGBA r g b a = unlift diskRGBA
    c = sin $ coef * ((rOuter - radius) ^ (2 :: Exp Int))
    in radius > rInner && radius < rOuter ?
           ( lift $ RGBA (c * r) (c * g) (c * b) (c * a)
           , rgba 0 0 0 0 )

interpolateDiskRadius :: Exp Position -> Exp Position -> Exp Float
interpolateDiskRadius pos newPos = let
    y = pos ^. _y
    yNew = newPos ^. _y
    r = norm pos
    rNew = norm newPos
    in (yNew * r - y * rNew) / (yNew - y)

-- Generate the sight rays ie. initial conditions for the integration
generateRay :: Exp (Float, Float) -> Exp Float -> Exp (M44 Float) -> Exp (V3 Float)
               -> Exp (V2 Float) -> Exp DIM2 -> Exp RaytracerState
generateRay (unlift -> (w, h) :: (Exp Float, Exp Float)) fov' proj pos offset idx = let
    Z :. y :. x = unlift idx
    V2' xOff yOff = unlift offset
    vec = V4' (fov' * ((fromIntegral x + xOff) / w - 0.5))
              (fov' * (0.5 - (fromIntegral y + yOff) / h) * h / w)
              (-1)
              0
    vel = normalize $ (proj !* vec) ^. _xyz
    accel = f (calculateh2 pos vel) pos
    in lift ((pos, vel, accel), rgba 0 0 0 0)

calculateh2 :: Exp Position -> Exp Velocity -> Exp Float
calculateh2 pos vel = quadrance $ pos `cross` vel

f :: Exp Float -> Exp Position -> Exp Acceleration
f coef x = coef / (norm x ^ (5 :: Exp Int)) *^ x

fst3 :: forall a b c. (Elt a, Elt b, Elt c) => Exp (a, b, c) -> Exp a
fst3 t = let (x, _, _) = unlift t :: (Exp a, Exp b, Exp c)
         in x

integrate :: Exp Float -> Exp Float -> Exp Float -> Exp PhotonState -> Exp PhotonState
integrate h hHalf coef photon = let
    (pos, vel, accel) = unlift photon
    posNext = pos + h *^ (vel + hHalf *^ accel)
    accelNext = f coef posNext
    velNext = vel + hHalf *^ (accel + accelNext)
    in lift (posNext, velNext, accelNext)

rayStep :: Exp DiskParams -> Exp Float -> Exp Float -> Exp Float -> Exp RaytracerState -> Exp RaytracerState
rayStep diskParams h hHalf coef raytracerState = let
    (photonState, currentColor) = unlift raytracerState
    newPhotonState = integrate h hHalf coef photonState

    newColor = let
        pos = fst3 photonState
        newPos = fst3 newPhotonState
        in signum pos ^. _y /= signum newPos ^. _y ?
              ( Raytracer.blend currentColor
                  $ diskColor' diskParams (interpolateDiskRadius pos newPos)
              , currentColor )

    in lift (newPhotonState, newColor)

doContinue :: Exp Float -> Exp RaytracerState -> Exp Bool
doContinue dotMax state = let
    photon = fst state
    (pos, vel, _ :: Exp Acceleration) = unlift photon
    dotProd = (pos `dot` vel) / (norm pos * norm vel)
    r2 = quadrance pos
    in r2 >= 1 && dotProd < dotMax

traceRay :: Exp Float -> Exp Float -> Exp DiskParams -> Exp RaytracerState -> Exp RaytracerState
traceRay h dotMax diskParams y = let
    coef = let
        (pos, vel, _ :: Exp Acceleration) = unlift $ fst y
        in -1.5 * calculateh2 pos vel
    in while (doContinue dotMax) (rayStep diskParams h (h / 2) coef) y

convertColour :: Exp (RGBA Float) -> Exp PixelRGBA8
convertColour (unlift . RGBA.clamp -> RGBA r g b _ :: RGBA (Exp Float)) = let
    RGB r' g' b' = unlift . SRGB.toRGB . lift $ RGB r g b :: RGB (Exp Float)
    in PixelRGBA8_ (round (255 * r'))
                   (round (255 * g'))
                   (round (255 * b'))
                   255

blackstarProgram :: Acc (Scalar Int) -> Acc SearchIndex -> Acc (Vector Star)
                    -> Acc (Scalar Scene) -> Acc (Scalar Camera) -> Acc (Matrix PixelRGBA8)
blackstarProgram division searchIndex stars scn' cam' = let
    scn = the scn'
    cam = the cam'

    lookup = starLookup (the division) searchIndex stars
        (starIntensity_ scn) (starSaturation_ scn)

    photonToRGBA state = let
        (pos :: Exp (V3 Float), vel :: Exp (V3 Float), _ :: Exp (V3 Float)) = unlift $ fst state
        bgColor = quadrance pos >= 1 ? (lookup vel, rgba 0 0 0 0)
        in Raytracer.blend (snd state) bgColor

    res = resolution_ scn
    (w, h) = unlift res
    res' = lift (fromIntegral w, fromIntegral h) :: Exp (Float, Float)
    matr = M.transpose $ LP.lookAt (position_ cam) (lookAt_ cam) (upVec_ cam)
    genRay x y = generateRay res' (fov_ cam) matr (position_ cam) $ V2' x y
    inner = compute . map (photonToRGBA . traceRay (stepSize_ scn) (stopThreshold_ scn) diskParams)
    render :: Exp Float -> Exp Float -> Acc (Matrix (RGBA Float))
    render xOffset yOffset = inner . compute $ generate (index2 h w) (genRay xOffset yOffset)

    -- Precalculate disk parameters
    rOuter = diskOuter_ scn
    rInner = diskInner_ scn
    diskCoef = pi / ((rOuter - rInner) ^ (2 :: Exp Int))
    RGB r g b = unlift . HSL.toRGB $ diskColor_ scn :: RGB (Exp Float)
    diskRGBA = lift $ RGBA r g b (diskOpacity_ scn) :: Exp (RGBA Float)
    diskParams = lift (diskRGBA, rOuter, rInner, diskCoef) :: Exp DiskParams

    average4 x1 x2 x3 x4 = 0.25 * (x1 + x2 + x3 + x4)
    img = supersampling_ scn ?|
        ( zipWith4 average4
            (render (1/8) (3/8))
            (render (-1/8) (-3/8))
            (render (-3/8) (1/8))
            (render (3/8) (-1/8))
        , render 0 0 )
    in map convertColour img