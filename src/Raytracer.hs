{-# LANGUAGE BangPatterns #-}

module Raytracer where

import Vision.Primitive
import qualified Vision.Image as I
import Linear hiding (lookAt, mult, trace)
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
generateRay !scn !(Z :. y' :. x') = (vel, pos ^+^ vel)
    where cam = camera scn
          pos = position cam
          xres = (fromIntegral . fst $ resolution cam) :: Double
          yres = (fromIntegral . snd $ resolution cam) :: Double
          matr = (L.lookAt pos (lookAt cam) (upVec cam)) ^. _m33
          vel  = normalize . (matr !*)
                 $ V3 (fov cam * ((fromIntegral x') / xres - 0.5))
                      (fov cam * ((fromIntegral y') / yres - 0.5) * yres/xres)
                      (-1)

render :: Scene -> I.RGB -> I.RGBDelayed
{-# INLINE render #-}
render !scn !tex = I.fromFunction (ix2 yres xres) (colorize tex . traceray' scn)
    where cam = camera scn
          (xres, yres) = resolution cam

sqrnorm :: V3 Double -> Double
{-# INLINE sqrnorm #-}
sqrnorm (V3 !x !y !z) = x*x + y*y + z*z

traceray :: Scene -> Point -> (V3 Double, V3 Double)
{-# INLINE traceray #-}
traceray !scn !pt = last . take (nSteps scn)
                  . takeWhile (\(_, p) -> sqrnorm p > 1)
                  . iterate (rk4 (stepSize scn) (fbinet h2)) $ ray
                  where ray@(vel, pos) = generateRay scn pt
                        h2 = sqrnorm (pos `cross` vel)

traceray' :: Scene -> Point -> (V3 Double, V3 Double)
{-# INLINE traceray' #-}
traceray' !scn !pt = rayFromSpherical . last . take (nSteps scn)
                   -- . takeWhile (\(_, (V4 !r _ _ _)) -> r > 1)
                   . iterate (rk4 (stepSize scn) schwarzGeodesic)
                   $ ray
                   where ray = rayToSpherical $ generateRay scn pt

rayToSpherical :: (V3 Double, V3 Double) -> (V4 Double, V4 Double)
{-# INLINE rayToSpherical #-}
rayToSpherical (!vel, !pos) = ( set _xyz vel' (V4 0 0 0 1)
                              , set _xyz pos' (V4 0 0 0 0) )
    where pos' = cartesianToSpherical pos
          vel' = sphericalInvJacobian pos' vel

rayFromSpherical :: (V4 Double, V4 Double) -> (V3 Double, V3 Double)
{-# INLINE rayFromSpherical #-}
rayFromSpherical (!vel, !pos) = ( sphericalJacobian pos3 $ vel ^. _xyz
                                , sphericalToCartesian pos3 )
    where pos3 = pos ^. _xyz

colorize :: I.RGB -> (V3 Double, V3 Double) -> I.RGBPixel
{-# INLINE colorize #-}
colorize !tex (!vel, !pos) = if sqrnorm pos < 2.25 then I.RGBPixel 0 0 0
                                                   else uvLookup tex vel

uv :: V3 Double -> (Double, Double)
{-# INLINE uv #-}
uv vec@(V3 !x !y !z) = (u, v) where
    u = 0.5 + (atan2 y x) / (2*pi)
    v = 0.5 - asin (z/r) / pi
    r = norm vec

clamp :: Ord a => a -> a -> a -> a
{-# INLINE clamp #-}
clamp mn mx = max mn . min mx

uvLookup :: I.RGB -> V3 Double -> I.RGBPixel
{-# INLINE uvLookup #-}
uvLookup !tex !vel = tex `I.index`
                      (ix2 (clamp 0 (h-1) . floor $ v * (fromIntegral h))
                           (clamp 0 (w-1) . floor $ u * (fromIntegral w)))
    where (Z :. h :. w) = I.shape tex
          (u, v) = uv vel

rk4 :: Additive f => Double -> ((f Double, f Double) -> (f Double, f Double))
                  -> (f Double, f Double) -> (f Double, f Double)
{-# INLINE rk4 #-}
rk4 !h !f !y = y `add`
    ((k1 `add` (k2 `mul` 2) `add` (k3 `mul` 2) `add` k4) `mul` (h/6))
    where k1 = f y
          k2 = f (y `add` (k1 `mul` (h/2)))
          k3 = f (y `add` (k2 `mul` (h/2)))
          k4 = f (y `add` (k3 `mul` h))

          add (!x, !z) (!u, !v) = (x ^+^ u, z ^+^ v)
          mul (!u, !v) !a = (u ^* a, v ^* a)

fbinet :: Double -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)
{-# INLINE fbinet #-}
fbinet !h2 (!vel, !pos) = (-1.5*h2 / ((sqrnorm pos)**2.5) *^ pos, vel)

-- The right hand sides of the Schwarzschild geodesic equations written down
-- explicitly
schwarzGeodesic :: (V4 Double, V4 Double) -> (V4 Double, V4 Double)
{-# INLINE schwarzGeodesic #-}
schwarzGeodesic (vel@(V4 !dr !dth !dphi !dt), V4 !r !th _ _) = (V4
    ((-dt*dt*w + dr*dr/w)/(2*r2) + r*w*(dth*dth + dphi*dphi*sin th**2))
    (dphi*dphi*sin th*cos th - 2*dr*dth/r)
    (-2*dphi*(dr/r + dth/tan th))
    (-dt*dr/(w*r2))
    , vel )
    where w = 1 - 1/r
          r2 = r*r

sphericalToCartesian :: V3 Double -> V3 Double
{-# INLINE sphericalToCartesian #-}
sphericalToCartesian (V3 !r !th !phi) = r *^ V3 (sin th*cos phi)
                                                (sin th*sin phi)
                                                (cos th)

cartesianToSpherical :: V3 Double -> V3 Double
{-# INLINE cartesianToSpherical #-}
cartesianToSpherical v@(V3 !x !y !z) = let r = norm v in
    V3 r (acos (z/r)) (atan2 y x)

-- Convert a velocity in Cartesian to spherical coordinates
sphericalInvJacobian :: V3 Double -> V3 Double -> V3 Double
{-# INLINE sphericalInvJacobian #-}
sphericalInvJacobian (V3 !r !th !phi) (V3 !dx !dy !dz) = V3
    (st * (cp*dx + sp*dy) + ct*dz)
    (ct/r * (cp*dx + sp*dy) - dz*st/r)
    ((cp*dy - sp*dx) / (r*st))
    where st = sin th
          ct = cos th
          sp = sin phi
          cp = cos phi

-- Convert a velocity in Cartesian to spherical coordinates
sphericalJacobian :: V3 Double -> V3 Double -> V3 Double
{-# INLINE sphericalJacobian #-}
sphericalJacobian (V3 !r !th !phi) (V3 !dr !dth !dphi) = V3
    (st*cp*dr + r*(ct*cp*dth - st*sp*dphi))
    (sp*st*dr + r*(ct*sp*dth + st*cp*dphi))
    (ct*dr - st*dth)
    where st = sin th
          ct = cos th
          sp = sin phi
          cp = cos phi
