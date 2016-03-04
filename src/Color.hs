{-# LANGUAGE BangPatterns #-}

module Color
    ( Rgb(Rgb)
    , Rgba(Rgba)
    , fromRGBPixel
    , fromRGBPixelWithAlpha
    , toRGBPixel
    , addAlpha
    , blend
    , bloom
    , gaussianBlur
    ) where

import qualified Data.Vector.Unboxed as U
import Vision.Primitive
import qualified Vision.Image as I

data Rgba = Rgba !Double !Double !Double !Double
data Rgb = Rgb !Double !Double !Double

fromRGBPixel :: I.RGBPixel -> Rgba
fromRGBPixel pxl = fromRGBPixelWithAlpha pxl 1

fromRGBPixelWithAlpha :: I.RGBPixel -> Double -> Rgba
fromRGBPixelWithAlpha (I.RGBPixel !r !g !b) alpha =
    Rgba (fromIntegral r) (fromIntegral g) (fromIntegral b) alpha

fromRGBPixel1 :: I.RGBPixel -> Rgb
fromRGBPixel1 (I.RGBPixel !r !g !b) =
    Rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)

toRGBPixel :: Rgba -> I.RGBPixel
toRGBPixel (Rgba !r !g !b _) = let convert = floor . max 0 . min 255 in
    I.RGBPixel (convert r) (convert g) (convert b)

toRGBPixel1 :: Rgb -> I.RGBPixel
toRGBPixel1 (Rgb !r !g !b) = let convert = floor . max 0 . min 255 in
    I.RGBPixel (convert r) (convert g) (convert b)

addAlpha :: Rgb -> Double -> Rgba
addAlpha (Rgb !r !g !b) !a = Rgba r g b a

blend :: Rgba -> Rgba -> Rgba
blend (Rgba !tr !tg !tb !ta) (Rgba !br !bg !bb !ba) = let
        a = ta + ba * (1 - ta)
        comp tc bc = if a == 0 then 0 else (tc*ta + bc*ba*(1-ta)) / a
    in Rgba (comp tr br) (comp tg bg) (comp tb bb) a

add :: Rgb -> Rgb -> Rgb
add (Rgb !r !g !b) (Rgb !r' !g' !b') = Rgb (r+r') (g+g') (b+b')

mul :: Double -> Rgb -> Rgb
mul !a (Rgb !r !g !b) = Rgb (a*r) (a*g) (a*b)

-- A somewhat fast Gaussian blur implementation using a separable kernel.
-- This is pretty sketchy and maybe not entirely correct. friday's Gaussian
-- blur filter doesn't currently support RGB images.
gaussianBlur :: Monad m => Int -> I.RGB -> m I.RGB
gaussianBlur !rad !src = let
    sh@(Z :. h :. w) = I.shape src

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

    convolve :: I.RGB -> (DIM2 -> U.Vector (Double, Int, Int))
                -> DIM2 -> I.RGBPixel
    convolve img !kern !ix = let
        !k = kern ix
        !n = norms U.! (U.length k - 1)
        in toRGBPixel1 . mul n $ U.foldl' (acc img) (Rgb 0 0 0) k

    acc :: I.RGB -> Rgb -> (Double, Int, Int) -> Rgb
    acc !img !pxl (!weight, !y, !x) = add pxl
        $ weight `mul` (fromRGBPixel1 $ img I.! ix2 y x)
    in do
        tmp <- I.computeP
            $ (I.fromFunction sh (convolve src kernH) :: I.RGBDelayed)
        I.computeP $ (I.fromFunction sh (convolve tmp kernV) :: I.RGBDelayed)

-- Apply Gaussian blur and add it to the image weighted by a constant
bloom :: Monad m => Double -> I.RGB -> m I.RGB
bloom strength src = do
    let sh@(Z :. _ :. w) = I.shape src
    tmp <- gaussianBlur (w `div` 20) src
    I.computeP $ (I.fromFunction sh
        (\ix -> toRGBPixel1 $ fromRGBPixel1 (src I.! ix) `add`
        (strength `mul` (fromRGBPixel1 $ tmp I.! ix))) :: I.RGBDelayed)
