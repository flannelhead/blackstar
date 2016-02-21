{-# LANGUAGE BangPatterns #-}

module Geometry where

type FourVector a = (a, a, a, a)

add :: Num a => FourVector a -> FourVector a -> FourVector a
{-# INLINE add #-}
add (!a, !b, !c, !d) (!x, !y, !z, !w) = (a+x, b+y, c+z, d+w)

mult :: Num a => FourVector a -> a -> FourVector a
{-# INLINE mult #-}
mult (!a, !b, !c, !d) !k = (a*k, b*k, c*k, d*k)

-- The right hand sides of the Schwarzschild geodesic equations written down
-- explicitly for comparing with the AD'd versions
schwarzGeodesic :: FourVector Double -> FourVector Double -> FourVector Double
{-# INLINE schwarzGeodesic #-}
schwarzGeodesic (!dt, !dr, !dth, !dphi) (_, !r, !th, _) = (
    -dt*dr / r',
    -dt*dt * (r-1)/(2*r**3) + dr*dr / (2*r') + dth*dth * (r-1)
     + dphi*dphi * (sin th)**2 * (r-1),
    dphi*dphi * sin th * cos th - 2*dr*dth / r,
    -2*dphi * (dr / r + dth / tan th))
    where r' = r * (r - 1)

-- Given a Cartesian 3-component direction vector, compute the corresponding
-- "light ray velocity" 4-vector in Cartesian coordinates
rayVelocity :: (Double, Double, Double) -> FourVector Double
{-# INLINE rayVelocity #-}
rayVelocity (!x, !y, !z) = (1, x/norm, y/norm, z/norm)
    where norm = sqrt(x*x + y*y + z*z)

schwarzToCartesian :: FourVector Double -> FourVector Double
schwarzToCartesian (!t, !r, !th, !phi) = (t, r * sin th * cos phi,
    r * sin th * sin phi, r * cos th)

cartesianToSchwarz :: FourVector Double -> FourVector Double
cartesianToSchwarz (!t, !x, !y, !z) = (t, r, acos (z / r), atan2 y x)
    where r = sqrt (x*x + y*y + z*z)
