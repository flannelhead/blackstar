{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable,
    Rank2Types, BangPatterns #-}

module Geometry where

import Numeric.AD.Mode.Reverse
import qualified Data.Vector.Unboxed as V

-- A custom datatype for a 4-vector. This is required for AD to work
-- (need to have the Traversable instance).
data FourVector a = FV !a !a !a !a
                    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Alias for the type of a metric
type Metric a = (Int, Int) -> FourVector a -> a

-- Convert a 4-vector to a list
toList :: FourVector a -> [a]
{-# INLINE toList #-}
toList (FV !a !b !c !d) = [a, b, c, d]

add :: Num a => FourVector a -> FourVector a -> FourVector a
{-# INLINE add #-}
add (FV !a !b !c !d) (FV !x !y !z !w) = FV (a+x) (b+y) (c+z) (d+w)

mult :: Num a => FourVector a -> a -> FourVector a
{-# INLINE mult #-}
mult !v !a = fmap (* a) v

-- Indexing into a 4-vector
idx :: Floating a => FourVector a -> Int -> a
{-# INLINE idx #-}
idx (FV !a  _  _  _) 0 = a
idx (FV  _ !a  _  _) 1 = a
idx (FV  _  _ !a  _) 2 = a
idx (FV  _  _  _ !a) 3 = a
idx _ _ = 0

-- The metric and connection components are internally stored as 1D vectors of
-- 10 elements. This is an index conversion function for that purpose
convIndex :: (Int, Int) -> Int
{-# INLINE convIndex #-}
convIndex (!i, !j) = if i > j then ix (j, i)
                              else ix (i, j)
    where ix (0, k) = k
          ix (1, k) = 3 + k
          ix (2, 2) = 7
          ix (2, 3) = 8
          ix (3, 3) = 9
          ix _ = -1

-- Given the components of the inverse metric and the metric derivatives,
-- compute a single Christoffel symbol
christoffel :: V.Vector Double -> Metric Double -> FourVector Double
               -> (Int, Int, Int) -> Double
{-# INLINE christoffel #-}
christoffel !dmetric !imetric !crd (!l, !j, !k) = sum (map term [0..3]) / 2
    where term !r = imetric (l, r) crd * (
                        (dmetric `V.unsafeIndex` (4 * convIndex (r, j) + k))
                      + (dmetric `V.unsafeIndex` (4 * convIndex (r, k) + j))
                      - (dmetric `V.unsafeIndex` (4 * convIndex (j, k) + r))
                      )

-- The set of indices of unique elements of a symmetric 4x4 matrix
symIndices :: [(Int, Int)]
symIndices = [ (0, 0), (0, 1), (0, 2), (0, 3),
                       (1, 1), (1, 2), (1, 3),
                               (2, 2), (2, 3),
                                       (3, 3) ]

-- Compute the right hand side of the geodesic equation
fgeodesic :: (forall a. Floating a => Metric a) -> Metric Double
             -> FourVector Double -> FourVector Double
             -> FourVector Double
{-# INLINE fgeodesic #-}
fgeodesic !metric !imetric !vel !crd = fmap fcomponent (FV (0 :: Int) 1 2 3)
    where fcomponent !l = -sum (zipWith (term l) symIndices coeffs)
          term !l (!j, !k) !c = c * (vel `idx` j) * (vel `idx` k)
              * christoffel dmetric imetric crd (l, j, k)
            -- Because of the symmetry of the Levi-Civita connection, the
            -- off-diagonal components are duplicated
          coeffs :: [Double]
          coeffs = [ 1, 2, 2, 2,
                        1, 2, 2,
                           1, 2,
                              1 ]
          -- Precompute the metric derivatives at crd
          -- TODO: experiment with having an unboxed vector here instead
          dmetric = V.fromList $ concatMap (\i -> toList (grad (metric i) crd))
              symIndices

-- The right hand sides of the Schwarzschild geodesic equations written down
-- explicitly for comparing with the AD'd versions
schwarzGeodesic :: FourVector Double -> FourVector Double -> FourVector Double
{-# INLINE schwarzGeodesic #-}
schwarzGeodesic (FV !dt !dr !dth !dphi) (FV _ !r !th _) = FV
    (-dt*dr / r')
    (-dt*dt * (r-1)/(2*r**3) + dr*dr / (2*r') + dth*dth * (r-1)
     + dphi*dphi * (sin th)**2 * (r-1))
    (dphi*dphi * sin th * cos th - 2*dr*dth / r)
    (-2*dphi * (dr / r + dth / tan th))
    where r' = r * (r - 1)

-- Given a Cartesian 3-component direction vector, compute the corresponding
-- "light ray velocity" 4-vector in Cartesian coordinates
rayVelocity :: (Double, Double, Double) -> FourVector Double
{-# INLINE rayVelocity #-}
rayVelocity (!x, !y, !z) = FV 1 (x/norm) (y/norm) (z/norm)
    where norm = sqrt(x*x + y*y + z*z)

-- The Schwarzschild metric with a Schwarzschild radius of 1
schwarz :: Floating a => Metric a
{-# INLINE schwarz #-}
schwarz (0, 0) (FV _ !r   _ _) = (r - 1) / r
schwarz (1, 1) (FV _ !r   _ _) = -r / (r - 1)
schwarz (2, 2) (FV _ !r   _ _) = -r**2
schwarz (3, 3) (FV _ !r !th _) = -(r * sin th)**2
schwarz (_, _) _ = 0

-- The inverse of the Schwarzschild metric
ischwarz :: Floating a => Metric a
{-# INLINE ischwarz #-}
ischwarz (!mu, !nu) !coords = if mu == nu then 1 / schwarz (mu, nu) coords
                                          else 0

schwarzToCartesian :: FourVector Double -> FourVector Double
schwarzToCartesian (FV !t !r !th !phi) = FV t
    (r * sin th * cos phi) (r * sin th * sin phi) (r * cos th)

cartesianToSchwarz :: FourVector Double -> FourVector Double
cartesianToSchwarz (FV !t !x !y !z) = FV t r (acos (z / r)) (atan2 y x)
    where r = sqrt (x*x + y*y + z*z)
