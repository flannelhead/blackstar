{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE DeriveGeneric #-}

module Data.Point2d where

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics
import Test.QuickCheck

data Point2d = Point2d Double Double deriving (Show, Eq, Ord, Generic)
instance NFData Point2d where rnf = genericRnf

pointAsList2d :: Point2d -> [Double]
pointAsList2d (Point2d x y) = [x, y]

distSqr2d :: Point2d -> Point2d -> Double
distSqr2d (Point2d x1 y1) (Point2d x2 y2) = let dx = x2 - x1
                                                dy = y2 - y1
                                            in  dx*dx + dy*dy

instance Arbitrary Point2d where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Point2d x y)
