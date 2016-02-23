{-# LANGUAGE BangPatterns #-}

module Geometry where

type FourVector = (Double, Double, Double, Double)

add :: FourVector -> FourVector -> FourVector
{-# INLINE add #-}
add (!a, !b, !c, !d) (!x, !y, !z, !w) = (a+x, b+y, c+z, d+w)

mult :: FourVector -> Double -> FourVector
{-# INLINE mult #-}
mult (!a, !b, !c, !d) !k = (a*k, b*k, c*k, d*k)

-- The right hand sides of the Schwarzschild geodesic equations written down
-- explicitly
schwarzGeodesic :: FourVector -> FourVector -> FourVector
{-# INLINE schwarzGeodesic #-}
schwarzGeodesic (!dt, !dr, !dth, !dphi) (_, !r, !th, _) =
    ( -dt*dr/(w*r2)
    , -dt*dt*w/(2*r2) + dr*dr/(2*w*r2) + r*w*(dth*dth + dphi*dphi*sinth**2)
    , dphi*dphi*sinth*cos th - 2*dr*dth/r
    , -2*dphi*(dr/r + dth*(cos th / sinth)) )
    where w = 1 - 1/r
          r2 = r*r
          -- sinth = if abs (sin th) < 1e-8 then signum th * 1e-8 else sin th
          sinth = sin th

schwarzToCartesian :: FourVector -> FourVector
{-# INLINE schwarzToCartesian #-}
schwarzToCartesian (!t, !r, !th, !phi) =
    ( t
    , r*sin th*cos phi
    , r*sin th*sin phi
    , r*cos th )

cartesianToSchwarz :: FourVector -> FourVector
{-# INLINE cartesianToSchwarz #-}
cartesianToSchwarz (!t, !x, !y, !z) = (t, r, acos (z/r), atan2 y x)
    where r = sqrt (x*x + y*y + z*z)

-- Convert a velocity in Cartesian to spherical coordinates
schwarzInvJacobian :: FourVector -> FourVector -> FourVector
{-# INLINE schwarzInvJacobian #-}
schwarzInvJacobian (_, !r, !th, !phi) (!dt, !dx, !dy, !dz) =
    ( dt
    , st * (cp*dx + sp*dy) + ct*dz
    , ct/r * (cp*dx + sp*dy) - dz*st/r
    , (cp*dy - sp*dx) / (r*st) )
    where st = sin th
          ct = cos th
          sp = sin phi
          cp = cos phi

-- Convert a velocity in Cartesian to spherical coordinates
schwarzJacobian :: FourVector -> FourVector -> FourVector
{-# INLINE schwarzJacobian #-}
schwarzJacobian (_, !r, !th, !phi) (!dt, !dr, !dth, !dphi) =
    ( dt
    , st*cp*dr + r*ct*cp*dth - r*st*sp*dphi
    , sp*st*dr + r*ct*sp*dth + r*st*cp*dphi
    , ct*dr - st*dth )
    where st = sin th
          ct = cos th
          sp = sin phi
          cp = cos phi

flatGeodesic :: FourVector -> FourVector -> FourVector
flatGeodesic _ _ = (0, 0, 0, 0)
