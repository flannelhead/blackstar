{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ImageUtil (bilinearFilter, bloom) where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Colour.RGB
import qualified Prelude as P

scalarMult :: Exp Float -> Exp (RGB Float) -> Exp (RGB Float)
scalarMult c (unlift -> RGB r g b :: RGB (Exp Float)) =
    lift $ RGB (r * c) (g * c) (b * c)

bilinearFilter :: Exp DIM2 -> Acc (Matrix (RGB Float)) -> Acc (Matrix (RGB Float))
bilinearFilter newShape@(unlift -> Z :. newHeight :. newWidth) (compute -> img) = let
    Z :. oldHeight :. oldWidth = unlift $ shape img
    xScale = fromIntegral oldWidth / fromIntegral newWidth :: Exp Float
    yScale = fromIntegral oldHeight / fromIntegral newHeight :: Exp Float

    pix :: Exp Int -> Exp Int -> Exp Float -> Exp (RGB Float)
    pix x y a = scalarMult a $ img ! index2 y x

    f (unlift -> Z :. i :. j) = let
        x = (fromIntegral j + 0.5) * xScale - 0.5 :: Exp Float
        y = (fromIntegral i + 0.5) * yScale - 0.5 :: Exp Float
        x' = max 0 . min (oldWidth - 2) $ floor x
        y' = max 0 . min (oldHeight - 2) $ floor y
        fracX = x - fromIntegral x' :: Exp Float
        fracY = y - fromIntegral y' :: Exp Float
        complementX = 1 - fracX :: Exp Float
        complementY = 1 - fracY :: Exp Float
        in scalarMult complementY (pix x' y'       complementX + pix (x' + 1) y'       fracX) +
           scalarMult fracY       (pix x' (y' + 1) complementX + pix (x' + 1) (y' + 1) fracX)

    in generate newShape f

type Stencil5x1 a = (Stencil3 a, Stencil5 a, Stencil3 a)
type Stencil1x5 a = (Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a, Stencil3 a)

convolve5x1 :: (Num a, Fractional a) => [Exp a] -> Stencil5x1 a -> Exp a
convolve5x1 kernel (_, (a,b,c,d,e), _)
    = 0.2 * (a + b + c + d + e)
    -- = P.sum $ P.zipWith (*) kernel [a,b,c,d,e]

convolve1x5 :: (Num a, Fractional a) => [Exp a] -> Stencil1x5 a -> Exp a
convolve1x5 kernel ((_,a,_), (_,b,_), (_,c,_), (_,d,_), (_,e,_))
    = 0.2 * (a + b + c + d + e)
    -- = P.sum $ P.zipWith (*) kernel [a,b,c,d,e]

gaussian5 :: [Exp (RGB Float)]
gaussian5 = [0.06136, 0.24477, 0.38774, 0.24477, 0.06136]

gaussianX :: Acc (Matrix (RGB Float)) -> Acc (Matrix (RGB Float))
gaussianX = stencil (convolve5x1 gaussian5) A.clamp

gaussianY :: Acc (Matrix (RGB Float)) -> Acc (Matrix (RGB Float))
gaussianY = stencil (convolve1x5 gaussian5) A.clamp

blur :: Acc (Matrix (RGB Float)) -> Acc (Matrix (RGB Float))
blur = compute . gaussianY . compute . gaussianX . compute

scaleDown :: Acc (Matrix (RGB Float)) -> Acc (Matrix (RGB Float))
scaleDown img = let
    Z :. h :. w = unlift $ shape img
    newShape = index2 (h `div` 2) (w `div` 2)
    in bilinearFilter newShape img

bloom :: Exp Float -> Exp Float -> Acc (Matrix (RGB Float)) -> Acc (Matrix (RGB Float))
bloom strength divider img = let
    imgDown = scaleDown . scaleDown . scaleDown . scaleDown $ scaleDown img
    in bilinearFilter (shape img) imgDown