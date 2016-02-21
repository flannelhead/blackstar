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
    ( -dt*dr / r'
    , -dt*dt * (r-1) / (2*r**3) + dr*dr / (2*r') + dth*dth * (r-1)
        + dphi*dphi * ((sin th)**2) * (r-1)
    , dphi*dphi * (sin th) * (cos th) - 2*dr*dth / r
    , -2*dphi * (dr / r + dth / tan th) )
    where r' = r * (r - 1)

schwarzToCartesian :: FourVector -> FourVector
{-# INLINE schwarzToCartesian #-}
schwarzToCartesian (!t, !r, !th, !phi) = (t, r * sin th * cos phi,
    r * sin th * sin phi, r * cos th)

cartesianToSchwarz :: FourVector -> FourVector
{-# INLINE cartesianToSchwarz #-}
cartesianToSchwarz (!t, !x, !y, !z) = (t, r, acos (z / r), atan2 y x)
    where r = sqrt (x*x + y*y + z*z)
