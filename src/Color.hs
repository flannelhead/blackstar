{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}

module Color
    ( RGB(RGB)
    , RGBA(RGBA)
    , HSV(HSV)
    , RGBImage
    , RGBImageDelayed
    , pngByteString
    , toTuple
    , addAlpha
    , dropAlpha
    , blend
    , addRGB
    , subRGB
    , mulRGB
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

type RGBImage = R.Array R.U DIM2 (Double, Double, Double)
type RGBImageDelayed = R.Array R.D DIM2 (Double, Double, Double)

data RGBA = RGBA Double Double Double Double
data RGB = RGB Double Double Double
data HSV = HSV Double Double Double

rgbImageToImage :: RGBImage -> DynamicImage
rgbImageToImage img = let
    (r, g, b) = RU.unzip3 img
    Z :. h :. w = R.extent img
    res = Image { imageWidth = w
                , imageHeight = h
                , imageData = S.convert
                    . U.map (floor . max 0 . min 255 . (* 255))
                    . R.toUnboxed . R.computeUnboxedS $ R.interleave3 r g b }
    in ImageRGB8 res

pngByteString :: RGBImage -> B.ByteString
pngByteString img = imageToPng $ rgbImageToImage img

toTuple :: RGB -> (Double, Double, Double)
{-# INLINE toTuple #-}
toTuple (RGB r g b) = (r, g, b)

hsvToRGB :: HSV -> RGB
{-# INLINE hsvToRGB #-}
hsvToRGB (HSV h s v) = let
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
    in RGB (r + m) (g + m) (b + m)

addAlpha :: RGB -> Double -> RGBA
{-# INLINE addAlpha #-}
addAlpha (RGB r g b) = RGBA r g b

dropAlpha :: RGBA -> RGB
{-# INLINE dropAlpha #-}
dropAlpha (RGBA r g b _) = RGB r g b

blend :: RGBA -> RGBA -> RGBA
{-# INLINE blend #-}
blend (RGBA tr tg tb ta) (RGBA br bg bb ba) = let
        a = ta + ba * (1 - ta)
        comp tc bc = if a == 0 then 0 else (tc*ta + bc*ba*(1-ta)) / a
    in RGBA (comp tr br) (comp tg bg) (comp tb bb) a

addRGB :: RGB -> RGB -> RGB
{-# INLINE addRGB #-}
addRGB (RGB r g b) (RGB r' g' b') = RGB (r+r') (g+g') (b+b')

subRGB :: RGB -> RGB -> RGB
{-# INLINE subRGB #-}
subRGB (RGB r g b) (RGB r' g' b') = RGB (r-r') (g-g') (b-b')

mulRGB :: Double -> RGB -> RGB
{-# INLINE mulRGB #-}
mulRGB a (RGB r g b) = RGB (a*r) (a*g) (a*b)
