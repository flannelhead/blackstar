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
              in (+ color) . (* coef) . toRGB $ hsl hue' (saturation' * sat) 0.5
            , color )
        in lift $ (newColor, idx + 1)

    gridShape = index3 division division division
    vecIndex = toIndex gridShape $ vecToIndex division nvel
    baseIndex = vecIndex * 27

    accumulateCell (unlift -> (color :: Exp (RGB Float), idx)) = let
        (startIndex, len) = unlift $ searchIndex !! (baseIndex + idx)
        newColor = fst . iterate len accumulatePixel
            $ lift (color, startIndex :: Exp Int)
        in lift $ (newColor, idx + 1)

    RGB r g b = unlift . fst . iterate 27 accumulateCell
        $ lift (RGB (0 :: Exp Float) 0 0, 0 :: Exp Int)
    in rgba r g b 0