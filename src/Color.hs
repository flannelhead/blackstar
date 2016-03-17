{-# LANGUAGE BangPatterns #-}

module Color
    ( RGB
    , RGBA
    , HSV
    , RGBImage
    , RGBImageDelayed
    , pngByteString
    , addAlpha
    , dropAlpha
    , blend
    , bloom
    , hsvToRGB ) where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S
import qualified Data.Array.Repa as R
import Data.Array.Repa.Index
import qualified Data.Array.Repa.Repr.Unboxed as RU
import qualified Data.ByteString.Lazy as B
import Data.Fixed (mod')
import Codec.Picture.Types
import Codec.Picture.Saving

type RGBImage = R.Array R.U DIM2 RGB
type RGBImageDelayed = R.Array R.D DIM2 RGB

type RGBA = (Double, Double, Double, Double)
type RGB = (Double, Double, Double)
type HSV = (Double, Double, Double)

rgbImageToImage :: RGBImage -> DynamicImage
rgbImageToImage img = let
    (r, g, b) = RU.unzip3 img
    Z :. h :. w = R.extent img
    res = Image { imageWidth = w
                , imageHeight = h
                , imageData = S.map (floor . max 0 . min 255 . (* 255))
                    . S.convert . R.toUnboxed
                    . R.computeUnboxedS $ R.interleave3 r g b }
    in ImageRGB8 res

pngByteString :: RGBImage -> B.ByteString
pngByteString img = imageToPng $ rgbImageToImage img

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
    in (r + m, g + m, b + m)

addAlpha :: RGB -> Double -> RGBA
{-# INLINE addAlpha #-}
addAlpha (!r, !g, !b) !a = (r, g, b, a)

dropAlpha :: RGBA -> RGB
dropAlpha (!r, !g, !b, _) = (r, g, b)

blend :: RGBA -> RGBA -> RGBA
{-# INLINE blend #-}
blend (!tr, !tg, !tb, !ta) (!br, !bg, !bb, !ba) = let
        a = ta + ba * (1 - ta)
        comp tc bc = if a == 0 then 0 else (tc*ta + bc*ba*(1-ta)) / a
    in (comp tr br, comp tg bg, comp tb bb, a)

-- Apply Gaussian blur and add it to the image weighted by a constant
doBloom :: Monad m => Int -> Double -> RGBImage -> m RGBImage
doBloom !rad !bloomStrength !src = let
    sh@(Z :. h :. w) = R.extent src

    kernel :: U.Vector (Double, Int)
    kernel = U.fromList
        [ let r' = fromIntegral r
              sigma = (fromIntegral rad / 3)
              a = sqrt bloomStrength
          in (a * exp (-(r'*r') / (2*sigma*sigma)) / (sqrt(2*pi)*sigma), r)
        | r <- [ -rad .. rad ] ]

    convolve !acc !img !ix = U.foldl' (acc img ix) 0 kernel
    accH !img (Z :. y :. x) !pxl (!weight, !r)
        | x' < 0 || x' >= w = pxl
        | otherwise = pxl + weight * (img `R.unsafeIndex` ix2 y x')
        where x' = x + r
    accV !img (Z :. y :. x) !pxl (!weight, !r)
        | y' < 0 || y' >= w = pxl
        | otherwise = pxl + weight * (img `R.unsafeIndex` ix2 y' x)
        where y' = y + r

    convolveH, convolveV, process :: Monad m => R.Array R.U DIM2 Double
                                  -> m (R.Array R.U DIM2 Double)
    convolveH img = R.computeUnboxedP $ R.fromFunction sh (convolve accH img)
    convolveV img = R.computeUnboxedP $ R.fromFunction sh (convolve accV img)
    process img = do
        img' <- convolveH img
        img'' <- convolveV img'
        R.computeUnboxedP $ R.fromFunction sh (\ix -> img `R.unsafeIndex` ix
                                                    + img'' `R.unsafeIndex` ix)

    (r, g, b) = RU.unzip3 src
    in do r' <- process r
          g' <- process g
          b' <- process b
          return $ RU.zip3 r' g' b'

bloom :: Monad m => Double -> RGBImage -> m RGBImage
bloom strength src = do
    let sh@(Z :. _ :. w) = R.extent src
    doBloom (w `div` 20) strength src
