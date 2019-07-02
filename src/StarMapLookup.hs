{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module StarMapLookup (starLookup) where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Colour.HSL as HSL
import Data.Array.Accelerate.Data.Colour.RGB
import Data.Array.Accelerate.Data.Colour.RGBA
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate.Linear.Vector
import Data.Array.Accelerate.Linear.Metric
import Prelude()

import StarMap

clamp_ :: Ord a => Exp a -> Exp a -> Exp a -> Exp a
clamp_ a b x = max a $ min b x

vecToIndex :: Exp Int -> Exp (V3 Float) -> Exp DIM3
vecToIndex division vec = let
    vecNormalized = vec ^+^ (constant $ V3 1 1 1)
    V3 x y z = unlift . fmap (clamp_ 0 (division - 1) . floor)
                 $ 0.5 * fromIntegral division *^ vecNormalized
    in index3 z y x

starLookup :: Exp Int -> Acc SearchIndex -> Acc (Vector Star) ->
              Exp Float -> Exp Float -> Exp (V3 Float) -> Exp (RGBA Float)
starLookup division searchIndex stars intensity saturation' vel = let
    w = 0.0005 :: Exp Float  -- width parameter of the Gaussian
    r2 = (20 * w) ^ (2 :: Exp Int)
    nvel = normalize vel
    scale = 1.0 / (2 * w ^ (2 :: Exp Int))

    accumulatePixel (unlift -> (color, idx)) = let
        (pos, mag, hue', sat) = unlift $ stars !! idx
        dp = pos `dot` nvel
        newColor = 1 - dp * dp < r2 && dp > 0 ?
            ( let
                -- Conversion from the log magnitude scale to linear brightness
                -- and a Gaussian intensity function. This determines the
                -- apparent size and brightness of the star.
                d2 = qd pos nvel
                val = (* intensity) . exp $ mag - d2 * scale
                coef = lift $ RGB val val val :: Exp (RGB Float)
              in (+ color) . (* coef) . toRGB . lift $ HSL hue' (saturation' * sat) 0.5
            , color )
        in lift $ (newColor, idx + 1)

    accumulateCell (unlift -> color :: Exp (RGB Float)) (unlift -> (startIndex, len)) = let
        newColor = fst . iterate (fromIntegral len) accumulatePixel
            $ lift (color, fromIntegral startIndex :: Exp Int)
        in newColor

    RGB r g b = unlift $ sfoldl accumulateCell (rgb 0 0 0) (vecToIndex division nvel) searchIndex
    in lift $ RGBA r g b (0 :: Exp Float)