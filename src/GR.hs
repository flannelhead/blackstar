{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable,
    Rank2Types #-}

module GR where

import Numeric.AD
import Data.Array.Repa hiding (map, toList)
import Data.Array.Repa.Repr.Unboxed

data FourVector a = FVect a a a a
                    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

comp :: Floating a => Int -> FourVector a -> a
comp 0 (FVect x _ _ _) = x
comp 1 (FVect _ x _ _) = x
comp 2 (FVect _ _ x _) = x
comp 3 (FVect _ _ _ x) = x
comp _ _ = 0

toList :: FourVector a -> [a]
toList (FVect a b c d) = [a, b, c, d]

type Metric a = (Int, Int) -> FourVector a -> a

symIndices :: [(Int, Int)]
symIndices = [ (0, 0), (0, 1), (0, 2), (0, 3),
                       (1, 1), (1, 2), (1, 3),
                               (2, 2), (2, 3),
                                       (3, 3) ]

convIndex :: (Int, Int) -> Int
convIndex (i, j) = if i > j then idx (j, i)
                               else idx (i, j)
    where idx (0, k) = k
          idx (1, k) = 3 + k
          idx (2, k) = 5 + k
          idx (3, k) = 6 + k
          idx _ = -1

christoffel :: (Unbox a, Floating a) => Array U DIM2 a -> Array U DIM1 a
                                     -> (Int, Int, Int) -> a
christoffel dmetric imetric (l, j, k) = sum (map sumTerm [0..3]) / 2
    where sumTerm r = (imetric ! (Z :. convIndex (l, r))) *
             (  (dmetric ! (Z :. convIndex (r, j) :. k))
              + (dmetric ! (Z :. convIndex (r, k) :. j))
              - (dmetric ! (Z :. convIndex (j, k) :. r)) )

christoffels :: (Unbox a, Floating a) => (forall b. Floating b => Metric b)
                                      -> Metric a -> FourVector a
                                      -> Array U DIM2 a
christoffels metric imetric crd =
    fromListUnboxed (Z :. (4 :: Int) :. (10 :: Int))
    [ christoffel metricDerivatives inverseComponents (l, j, k)
      | l <- [0..3], (j, k) <- symIndices ]
    where
        inverseComponents = fromListUnboxed (Z :. (10 :: Int))
            [ imetric idx crd | idx <- symIndices ]
        metricDerivatives = fromListUnboxed (Z :. (10 :: Int) :. (4 :: Int))
            $ concat [ toList (grad (metric idx) crd) | idx <- symIndices ]

schwarz :: Floating a => Metric a
schwarz (0, 0) (FVect _ r  _ _) = 1 - 1/r
schwarz (1, 1) (FVect _ r  _ _) = -1 / (1 - 1/r)
schwarz (2, 2) (FVect _ r  _ _) = -r**2
schwarz (3, 3) (FVect _ r th _) = -(r * sin th)**2
schwarz (_, _) _ = 0

ischwarz :: Floating a => Metric a
ischwarz (mu, nu) coords = if mu == nu
                                   then 1 / schwarz (mu, nu) coords
                                   else 0
