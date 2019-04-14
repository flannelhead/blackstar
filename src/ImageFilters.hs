{-# LANGUAGE BangPatterns #-}

module ImageFilters (bloom, supersample) where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Massiv.Array
import Data.Massiv.Array.Manifest.Vector
import Data.Massiv.Array.Unsafe
import Data.Massiv.Array.IO
import Control.Monad (replicateM_)
import Control.Applicative (liftA2)
import Graphics.ColorSpace

ix1d :: Int -> Int -> Int -> Int
{-# INLINE ix1d #-}
ix1d !w !y !x = y*w + x

add :: Pixel RGB Double -> Pixel RGB Double -> Pixel RGB Double
add = liftA2 (+)
sub :: Pixel RGB Double -> Pixel RGB Double -> Pixel RGB Double
sub = liftA2 (-)
mul :: Double -> Pixel RGB Double -> Pixel RGB Double
mul a = fmap (a *)

boxBlur :: Int -> Int -> Image U RGB Double -> IO (Image U RGB Double)
boxBlur !r !passes img = let
    myDims@(h :. w) = size img
    rows' = U.enumFromN (0 :: Int) h
    cols' = U.enumFromN (0 :: Int) w

    -- Functions to safely index a vector representing an image with specialized
    -- horizontal / vertical bound checks. Out of bounds indices return a black
    -- pixel.
    {-# INLINE ixh #-}
    {-# INLINE ixv #-}
    {-# INLINE ix1d' #-}
    ix1d' = ix1d w
    ixh v y x
        | x < 0 || x >= w = PixelRGB 0 0 0
        | otherwise = U.unsafeIndex v $ ix1d' y x
    ixv v x y
        | y < 0 || y >= h = PixelRGB 0 0 0
        | otherwise = U.unsafeIndex v $ ix1d' y x

    -- Normalize by the "width" of the kernel
    normFactor :: Double
    {-# INLINE normFactor #-}
    normFactor = 1 / (2*fromIntegral r + 1)

    {-# INLINE blur #-}
    blur writeToVec crds ix1df readf vecIn y = let
        {-# INLINE pix #-}
        -- A function to yield a pixel from the image vector
        pix = readf vecIn y
        -- Starting value
        startVal = U.foldl1' add . U.map pix . U.unsafeTake r $ crds
        {-# INLINE accumulate #-}
        accumulate !rgb x = do
            let newRGB =  (rgb `add` pix (x+r)) `sub` pix (x-r)
            _ <- writeToVec (ix1df y x) $ mul normFactor newRGB
            return newRGB
        -- Sweep over the row / col of the image
        in U.foldM'_ accumulate startVal crds
    in do
        mv <- U.thaw $ toVector img
        let wrt = MU.unsafeWrite mv
        replicateM_ passes $ do
            -- First blur horizontally
            tmp1 <- U.freeze mv
            U.mapM_ (blur wrt cols' ix1d' ixh tmp1) rows'
            -- Then vertically
            tmp2 <- U.freeze mv
            U.mapM_ (blur wrt rows' (flip ix1d') ixv tmp2) cols'
        out <- U.unsafeFreeze mv
        return $ fromVector Par myDims out

bloom :: Double -> Int -> Image U RGB Double -> IO (Image U RGB Double)
bloom strength divider img = do
    let myDims@(_ :. w) = size img
    blurred <- boxBlur (w `div` divider) 3 img
    return . makeArrayR U Par myDims
        $ \ix -> img `unsafeIndex` ix `add`
                 mul strength (blurred `unsafeIndex` ix)

supersample :: Image U RGB Double -> Image U RGB Double
supersample img = let
    h :. w = size img
    {-# INLINE pix #-}
    pix y x = img `unsafeIndex` (y :. x)
    {-# INLINE f #-}
    f (y :. x) = mul 0.25
        $ pix (2*y) (2*x) `add` pix (2*y+1) (2*x) `add` pix (2*y) (2*x+1)
                          `add` pix (2*y+1) (2*x+1)
    in makeArrayR U Par (Sz ((h `div` 2) :. (w `div` 2))) f
