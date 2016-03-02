{-# LANGUAGE BangPatterns #-}

module Color where

import qualified Vision.Image as I

data Rgba = Rgba !Double !Double !Double !Double

fromRGBPixel :: I.RGBPixel -> Rgba
fromRGBPixel (I.RGBPixel !r !g !b) =
    Rgba (fromIntegral r) (fromIntegral g) (fromIntegral b) 1

toRGBPixel :: Rgba -> I.RGBPixel
toRGBPixel (Rgba !r !g !b _) = let convert = max 0 . min 255 . floor in
    I.RGBPixel (convert r) (convert g) (convert b)

blend :: Rgba -> Rgba -> Rgba
blend (Rgba !tr !tg !tb !ta) (Rgba !br !bg !bb !ba) = let
        a = ta + ba * (1 - ta)
        comp tc bc = if a == 0 then 0 else (tc*ta + bc*ba*(1-ta)) / a
    in Rgba (comp tr br) (comp tg bg) (comp tb bb) a
