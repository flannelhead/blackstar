{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable,
    Rank2Types, BangPatterns #-}

module GR where

import Numeric.AD.Mode.Reverse
import Data.Array.Repa hiding (map, toList)

data FourVector a = FVect !a !a !a !a
                    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

toList :: FourVector a -> [a]
{-# INLINE toList #-}
toList (FVect a b c d) = [a, b, c, d]

type Metric a = (Int, Int) -> FourVector a -> a

symIndices :: [(Int, Int)]
symIndices = [ (0, 0), (0, 1), (0, 2), (0, 3),
                       (1, 1), (1, 2), (1, 3),
                               (2, 2), (2, 3),
                                       (3, 3) ]

convIndex :: (Int, Int) -> Int
{-# INLINE convIndex #-}
convIndex (!i, !j) = if i > j then idx (j, i)
                               else idx (i, j)
    where idx (0, k) = k
          idx (1, k) = 3 + k
          idx (2, k) = 5 + k
          idx (3, k) = 6 + k
          idx _ = -1

christoffel :: Array U DIM2 Double -> Array U DIM1 Double
               -> (Int, Int, Int) -> Double
{-# INLINE christoffel #-}
christoffel !dmetric !imetric (!l, !j, !k) = sum (map term [0..3]) / 2
    where term !r = (imetric ! (Z :. convIndex (l, r))) *
                    ((dmetric ! (Z :. convIndex (r, j) :. k))
                     + (dmetric ! (Z :. convIndex (r, k) :. j))
                     - (dmetric ! (Z :. convIndex (j, k) :. r)))

christoffels :: (forall a. Floating a => Metric a)
                -> Metric Double -> FourVector Double
                -> Array U DIM2 Double
christoffels !metric !imetric !crd =
    fromListUnboxed (Z :. (4 :: Int) :. (10 :: Int))
    [ christoffel metricDerivatives inverseComponents (l, j, k)
      | l <- [0..3], (j, k) <- symIndices ]
    where
        inverseComponents = fromListUnboxed (Z :. (10 :: Int))
            [ imetric idx crd | idx <- symIndices ]
        metricDerivatives = fromListUnboxed (Z :. (10 :: Int) :. (4 :: Int))
            $ concat [ toList (grad (metric idx) crd) | idx <- symIndices ]

schwarz :: Floating a => Metric a
{-# INLINE schwarz #-}
schwarz (0, 0) (FVect _ !r   _ _) = 1 - 1/r
schwarz (1, 1) (FVect _ !r   _ _) = -1 / (1 - 1/r)
schwarz (2, 2) (FVect _ !r   _ _) = -r**2
schwarz (3, 3) (FVect _ !r !th _) = -(r * sin th)**2
schwarz (_, _) _ = 0

ischwarz :: Floating a => Metric a
{-# INLINE ischwarz #-}
ischwarz (!mu, !nu) !coords = if mu == nu
                                   then 1 / schwarz (mu, nu) coords
                                   else 0
