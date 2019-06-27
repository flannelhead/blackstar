{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Raytracer (render, writeImg, BlackstarProgram, compile) where

import Codec.Picture
import Control.Lens hiding (use)
import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.Data.Colour.HSL as HSL
import Data.Array.Accelerate.Data.Colour.RGB
import Data.Array.Accelerate.Data.Colour.RGBA as RGBA
import Data.Array.Accelerate.Data.Colour.SRGB as SRGB
import Data.Array.Accelerate.Linear.Vector
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Matrix as M
import Data.Array.Accelerate.Linear.Projection as LP
import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate.Linear.V4
import Data.Array.Accelerate.IO.Codec.Picture
import qualified Prelude as P

import StarMap
import StarMapLookup
import ConfigFile

type Position = V3 Float
type Velocity = V3 Float
type Acceleration = V3 Float
type PhotonState = (Position, Velocity, Acceleration)
type RaytracerState = (PhotonState, RGBA Float)
type DiskParams = (RGBA Float, Float, Float, Float)
type BlackstarProgram = Scalar Scene -> Scalar Camera -> Matrix PixelRGBA8

writeImg :: Image PixelRGB8 -> P.FilePath -> P.IO ()
writeImg img path = writePng path img

blend' :: P.Num a => RGBA a -> RGBA a -> RGBA a
blend' src dst = let
    RGBA sr sg sb sa = src
    RGBA dr dg db da = dst
    g s d = s + d * (1 - sa)
    in RGBA (g sr dr) (g sg dg) (g sb db) (g sa da)

blend :: Exp (RGBA Float) -> Exp (RGBA Float) -> Exp (RGBA Float)
blend = lift2 (blend' :: RGBA (Exp Float) -> RGBA (Exp Float) -> RGBA (Exp Float))

diskColor' :: Exp DiskParams -> Exp Float -> Exp (RGBA Float)
diskColor' (unlift -> (diskRGBA :: Exp (RGBA Float), rOuter, rInner, coef)) radius = let
    RGBA r g b a = unlift diskRGBA
    c = sin $ coef * ((rOuter - radius) ^ (2 :: Exp Int))
    in radius > rInner && radius < rOuter ?
           ( rgba (c * r) (c * g) (c * b) (c * a)
           , rgba 0 0 0 0 )

interpolateDiskRadius :: Exp Position -> Exp Position -> Exp Float
interpolateDiskRadius pos newPos = let
    y = pos ^. _y
    yNew = newPos ^. _y
    r = sqrt $ quadrance pos
    rNew = sqrt $ quadrance newPos
    in (yNew * r - y * rNew) / (yNew - y)

-- Generate the sight rays ie. initial conditions for the integration
generateRay :: Exp (Int, Int) -> Exp Camera -> Exp DIM2 -> Exp RaytracerState
generateRay (unlift -> (w', h') :: (Exp Int, Exp Int)) cam idx = let
    w = fromIntegral w'
    h = fromIntegral h'
    Z :. y :. x = unlift idx
    fov' = fov_ cam
    pos = position_ cam
    matr = M.transpose $ LP.lookAt pos (lookAt_ cam) (upVec_ cam)
    vec = V4' (fov' * (fromIntegral x / w - 0.5))
              (fov' * (0.5 - fromIntegral y / h) * h / w)
              (-1)
              0
    vel = normalize $ (matr !* vec) ^. _xyz
    accel = f (calculateh2 pos vel) pos
    in lift ((pos, vel, accel), rgba 0 0 0 0)

calculateh2 :: Exp Position -> Exp Velocity -> Exp Float
calculateh2 pos vel = quadrance $ pos `cross` vel

f :: Exp Float -> Exp Position -> Exp Acceleration
f h2 x = -1.5 * h2 / (norm x ^ (5 :: Exp Int)) *^ x

fst3 :: forall a b c. (Elt a, Elt b, Elt c) => Exp (a, b, c) -> Exp a
fst3 t = let (x, _, _) = unlift t :: (Exp a, Exp b, Exp c)
         in x

integrate :: Exp Float -> Exp Float -> Exp PhotonState -> Exp PhotonState
integrate h h2 photon = let
    (pos, vel, accel) = unlift photon
    posNext = pos + h *^ (vel + (h / 2) *^ accel)
    accelNext = f h2 posNext
    velNext = vel + (h / 2) *^ (accel + accelNext)
    in lift (posNext, velNext, accelNext)

rayStep :: Exp DiskParams -> Exp Float -> Exp Float -> Exp RaytracerState -> Exp RaytracerState
rayStep diskParams h h2 raytracerState = let
    (photonState, currentColor) = unlift raytracerState
    newPhotonState = integrate h h2 photonState

    newColor = let
        pos = fst3 photonState
        newPos = fst3 newPhotonState
        in signum pos ^. _y /= signum newPos ^. _y ?
              ( Raytracer.blend currentColor
                  $ diskColor' diskParams (interpolateDiskRadius pos newPos)
              , currentColor )

    in lift (newPhotonState, newColor)

traceRay :: Exp Scene -> Exp RaytracerState -> Exp RaytracerState
traceRay scn y = let
    h = stepSize_ scn
    h2 = let
        (pos, vel, _ :: Exp Acceleration) = unlift $ fst y
        in calculateh2 pos vel
    dotMax = stopThreshold_ scn
    rOuter = diskOuter_ scn
    rInner = diskInner_ scn
    diskCoef = pi / ((rOuter - rInner) ^ (2 :: Exp Int))
    RGB r g b = unlift . HSL.toRGB $ diskColor_ scn :: RGB (Exp Float)
    diskRGBA = rgba r g b (diskOpacity_ scn)
    diskParams = lift (diskRGBA, rOuter, rInner, diskCoef)

    doContinue :: Exp RaytracerState -> Exp Bool
    doContinue state = let
        photon = fst state
        (pos, vel, _ :: Exp Acceleration) = unlift photon
        dotProd = (pos `dot` vel) / (norm pos * norm vel)
        r2 = quadrance pos
        in r2 >= 1 && dotProd < dotMax

    in while doContinue (rayStep diskParams h h2) y

convertColour :: Exp (RGBA Float) -> Exp PixelRGBA8
convertColour (unlift . RGBA.clamp -> RGBA r g b _ :: RGBA (Exp Float)) = let
    RGB r' g' b' = unlift . SRGB.toRGB . lift $ RGB r g b :: RGB (Exp Float)
    in PixelRGBA8_ (round (255 * r'))
                   (round (255 * g'))
                   (round (255 * b'))
                   255

program :: StarGrid -> Acc (Scalar Scene) -> Acc (Scalar Camera) -> Acc (Matrix PixelRGBA8)
program stargrid scn' cam' = let
    scn = the scn'
    cam = the cam'
    res = resolution_ scn
    (w, h) = unlift res
    rays = generate (index2 h w) (generateRay res cam)

    StarGrid division searchindex stars = stargrid
    lookup = starLookup (constant division) (use searchindex) (use stars)
        (starIntensity_ scn) (starSaturation_ scn)

    photonToRGBA state = let
        (pos :: Exp (V3 Float), vel :: Exp (V3 Float), _ :: Exp (V3 Float)) = unlift $ fst state
        bgColor = quadrance pos >= 1 ? (lookup vel, rgba 0 0 0 0)
        in Raytracer.blend (snd state) bgColor

    in map (convertColour . photonToRGBA . traceRay scn) rays

compile :: StarGrid -> BlackstarProgram
compile grid = CPU.runN $ program grid

toArr :: Elt a => a -> Scalar a
toArr x = fromList Z [x]

render :: BlackstarProgram -> Scene -> Camera -> Image PixelRGB8
render prog scn cam = convertRGB8 . ImageRGBA8 . imageOfArray $ prog (toArr scn) (toArr cam)