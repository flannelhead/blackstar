{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

module Color
    ( RGB
    , RGBA
    , RGBImage
    , RGBImageDelayed
    , HSV
    , addAlpha
    , dropAlpha
    , blend
    , bloom
    , gaussianBlur
    , hsvToRGB ) where

import qualified Data.Vector.Unboxed as U
import Data.Array.Repa as R
import Data.Fixed (mod')

type RGBImage = R.Array R.U DIM2 RGB
type RGBImageDelayed = R.Array R.D DIM2 RGB

type RGBA = (Double, Double, Double, Double)
type RGB = (Double, Double, Double)
type HSV = (Double, Double, Double)

hsvToRGB :: HSV -> RGB
hsvToRGB (!h, !s, !v) = let
    c = v * s
    h' = h / 60
    x = c * (1 - abs ((h' `mod'` 2) - 1))
    m = v - c
    rgb h'' | h'' < 1 = (c, x, 0)
            | h'' < 2 = (x, c, 0)
            | h'' < 3 = (0, c, x)
            | h'' < 4 = (0, x, c)
            | h'' < 5 = (x, 0, c)
            | h'' < 6 = (c, 0, x)
            | otherwise = (0, 0, 0)
    (r, g, b) = rgb h'
    in (255 * (r + m), 255 * (g + m), 255 * (b + m))

addAlpha :: RGB -> Double -> RGBA
addAlpha (!r, !g, !b) !a = (r, g, b, a)

dropAlpha :: RGBA -> RGB
dropAlpha (!r, !g, !b, _) = (r, g, b)

blend :: RGBA -> RGBA -> RGBA
blend (!tr, !tg, !tb, !ta) (!br, !bg, !bb, !ba) = let
        a = ta + ba * (1 - ta)
        comp tc bc = if a == 0 then 0 else (tc*ta + bc*ba*(1-ta)) / a
    in (comp tr br, comp tg bg, comp tb bb, a)

add :: RGB -> RGB -> RGB
add (!r, !g, !b) (!r', !g', !b') = (r+r', g+g', b+b')

mul :: Double -> RGB -> RGB
mul !a (!r, !g, !b) = (a*r, a*g, a*b)

-- A somewhat fast Gaussian blur implementation using a separable kernel.
-- This is pretty sketchy and maybe not entirely correct. friday's Gaussian
-- blur filter doesn't currently support RGB images.
gaussianBlur :: Monad m => Int -> RGBImage -> m RGBImage
gaussianBlur !rad !src = let
    sh@(Z :. h :. w) = R.extent src

    kernel :: U.Vector (Double, Int)
    kernel = U.fromList
        [ let r' = fromIntegral r
              sigma = (fromIntegral rad / 3)
          in (exp (-(r'*r') / (2*sigma*sigma)) / (sqrt(2*pi)*sigma), r)
        | r <- [ -rad .. rad ] ]

    norms :: U.Vector Double
    norms = U.fromList [ 1 / (U.sum . U.take len . U.map fst $ kernel)
                       | len <- [ 1 .. 2*rad+1 ] ]

    kernH, kernV :: DIM2 -> U.Vector (Double, Int, Int)
    kernH !(Z :. y :. x) = U.filter (\(_, _, x') -> x' >= 0 && x' < w)
        $ U.map (\(a, dx) -> (a, y, x+dx)) kernel
    kernV !(Z :. y :. x) = U.filter (\(_, y', _) -> y' >= 0 && y' < h)
        $ U.map (\(a, dy) -> (a, y+dy, x)) kernel

    convolve :: RGBImage -> (DIM2 -> U.Vector (Double, Int, Int))
                -> DIM2 -> RGB
    convolve img !kern !ix = let
        !k = kern ix
        !n = norms U.! (U.length k - 1)
        in mul n $ U.foldl' (acc img) (0, 0, 0) k

    acc :: RGBImage -> RGB -> (Double, Int, Int) -> RGB
    acc !img !pxl (!weight, !y, !x) = add pxl
        $ weight `mul` (img R.! ix2 y x)
    in do
        tmp <- R.computeUnboxedP
            $ (R.fromFunction sh (convolve src kernH))
        R.computeUnboxedP $ (R.fromFunction sh (convolve tmp kernV))

-- Apply Gaussian blur and add it to the image weighted by a constant
bloom :: Monad m => Double -> RGBImage -> m RGBImage
bloom strength src = do
    let sh@(Z :. _ :. w) = R.extent src
    tmp <- gaussianBlur (w `div` 20) src
    R.computeUnboxedP $ (R.fromFunction sh
        (\ix -> (src R.! ix) `add` (strength `mul` (tmp R.! ix))))
