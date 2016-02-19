{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable,
    Rank2Types #-}

module GR where
import Numeric.AD

data FourVector a = FVect a a a a
                    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

comp :: Floating a => Int -> FourVector a -> a
comp 0 (FVect x _ _ _) = x
comp 1 (FVect _ x _ _) = x
comp 2 (FVect _ _ x _) = x
comp 3 (FVect _ _ _ x) = x
comp _ _ = 0

type Metric a = (Int, Int) -> FourVector a -> a

schwarzschild :: Floating a => Metric a
schwarzschild (0, 0) (FVect _ r  _ _) = 1 - 1/r
schwarzschild (1, 1) (FVect _ r  _ _) = -1 / (1 - 1/r)
schwarzschild (2, 2) (FVect _ r  _ _) = -r**2
schwarzschild (3, 3) (FVect _ r th _) = -(r * sin th)**2
schwarzschild (_, _) _ = 0

ischwarzschild :: Floating a => Metric a
ischwarzschild (mu, nu) coords = if mu == nu
                                   then 1 / schwarzschild (mu, nu) coords
                                   else 0

christoffel :: Floating a => (forall b. Floating b => Metric b) -> Metric a
                             -> (Int, Int, Int) -> FourVector a -> a
christoffel metric imetric (l, j, k) crd = sum (map sumTerm [0..3]) / 2
    where sumTerm r = imetric (l, r) crd *
             (comp k (grad (metric (r, j)) crd)
              + comp j (grad (metric (r, k)) crd)
              - comp r (grad (metric (j, k)) crd))

