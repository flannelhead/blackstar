{-# LANGUAGE BangPatterns #-}

module Color where

import Data.Word (Word8, Word16)
import Data.List (foldl')
import Vision.Primitive
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

gaussianBlur :: Int -> I.RGB -> I.RGBDelayed
gaussianBlur rad src = let
    sh@(Z :. h :. w) = I.shape src

    fromDouble :: Double -> Word8
    fromDouble = floor . clamp
    clamp = max 0 . min 255

    add :: I.RGBPixel -> I.RGBPixel -> I.RGBPixel
    add (I.RGBPixel r g b) (I.RGBPixel r' g' b') = let
        add' :: Word8 -> Word8 -> Word8
        add' x y = fromIntegral (fromIntegral x + fromIntegral y :: Word16)
        in I.RGBPixel (add' r r') (add' g g') (add' b b')

    mul :: Double -> I.RGBPixel -> I.RGBPixel
    mul a (I.RGBPixel r g b) = I.RGBPixel
        (fromDouble $ a*fromIntegral r)
        (fromDouble $ a*fromIntegral g)
        (fromDouble $ a*fromIntegral b)

    points (Z :. y :. x) = [ Z :. py :. px
                           | px <- [ max 0 (x-rad) .. min (w-1) (x+rad) ]
                           , py <- [ max 0 (y-rad) .. min (h-1) (y+rad) ] ]

    sigma2 = (fromIntegral rad / 3)**2
    kernel :: Double -> Double
    kernel r2 = exp (-r2 / 2*sigma2) / (2*pi*sigma2)

    getR2 (Z :. y :. x) (Z :. y' :. x') = let
        dx = fromIntegral x - fromIntegral x'
        dy = fromIntegral y - fromIntegral y' in dx*dx + dy*dy

    acc :: DIM2 -> I.RGBPixel -> DIM2 -> I.RGBPixel
    acc ix pxl ixCur = add pxl $ kernel (getR2 ix ixCur) `mul` (src I.! ixCur)

    in I.fromFunction sh (\ix -> foldl' (acc ix) (I.RGBPixel 0 0 0) (points ix))
