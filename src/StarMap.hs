{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module StarMap
    ( Star, StarTree, StoredStarTree
    , readMapFromFile, treeToByteString, readTreeFromFile
    , buildStarTree, starLookup ) where

import Control.Monad
import Data.Word
import Data.Char
import Data.Foldable
import qualified Data.ByteString as B
import Data.Serialize as S
import Data.KdMap.Static
import Linear as L
import Graphics.Image as I
import Graphics.Image.Interface

import Util

type Star = (V3 Double, (Int, Double, Double))
type StarTree = KdMap Double (V3 Double) (Int, Double, Double)
type StoredStar = (V3 Double, (Int, Char))
type StoredStarTree = KdMap Double (V3 Double) (Int, Char)

instance Serialize StoredStarTree
instance Serialize (TreeNode Double (V3 Double) (Int, Char))

-- We can't serialize functions but let's hack around it so that we can
-- serialize the KdMap anyway
instance Serialize (SquaredDistanceFn Double (V3 Double)) where
    put _ = put (0 :: Word8)
    get = skip 1 >> return (defaultSqrDist toList)

instance Serialize (PointAsListFn Double (V3 Double)) where
    put _ = put (0 :: Word8)
    get = skip 1 >> return toList

-- Parse the star list in the binary format specified at
-- http://tdc-www.harvard.edu/software/catalogs/ppm.entry.html
readMap :: Get [StoredStar]
readMap = do
    -- Skip the header
    skip 28
    nBytes <- remaining
    replicateM (nBytes `div` 28) $ do
        ra <- getFloat64be
        dec <- getFloat64be
        spectral <- getWord8
        skip 1
        mag <- getInt16be
        skip 8
        return ( raDecToCartesian ra dec
               , (fromIntegral mag, chr $ fromIntegral spectral) )

starColor' :: (Int, Char) -> (Int, Double, Double)
starColor' (mag, ch) = let (h, s) = starColor ch in (mag, h, s)

-- Some nice colour values for different spectral types
starColor :: Char -> (Double, Double)
starColor 'O' = (0.631, 0.39)
starColor 'B' = (0.628, 0.33)
starColor 'A' = (0.622, 0.21)
starColor 'F' = (0.650, 0.03)
starColor 'G' = (0.089, 0.09)
starColor 'K' = (0.094, 0.29)
starColor 'M' = (0.094, 0.56)
starColor _   = (0, 0)

raDecToCartesian :: Double -> Double -> V3 Double
raDecToCartesian ra dec = V3 (cos dec*cos ra) (cos dec*sin ra) (sin dec)

readMapFromFile :: FilePath -> IO (Either String [StoredStar])
readMapFromFile path = do
    ebs <- readSafe path
    return $ ebs >>= runGet readMap

readTreeFromFile :: FilePath -> IO (Either String StarTree)
readTreeFromFile path = do
    ebs <- readSafe path
    return $ fmap starColor' <$> (S.decode =<< ebs)

treeToByteString :: StoredStarTree -> B.ByteString
treeToByteString = S.encode

buildStarTree :: [StoredStar] -> StoredStarTree
buildStarTree = build toList

starLookup :: StarTree -> Double -> Double -> V3 Double -> Pixel RGB Double
{-# INLINE starLookup #-}
starLookup starmap intensity saturation vel = let
    -- The magnitude value tells about the intensity of the star. The
    -- brighter the star, the smaller the magnitude. These constants are
    -- used for adjusting the dynamics of the rendered celestial sphere.
    max_brightness = 950 -- the "maximum brightness" magnitude
    dynamic = 50         -- "dynamic range": magnitude change that doubles intensity
    w = 0.0005           -- width parameter of the gaussian function

    nvel = L.normalize vel
    stars = inRadius starmap (20 * w) nvel

    renderPixel (pos, (mag, hue, sat)) = let
        d2 = qd pos nvel
        a = log 2 / dynamic
        -- Conversion from the log magnitude scale to linear brightness
        -- and a Gaussian intensity function. This determines the apparent size
        -- and brightness of the star.
        val = (* intensity) . min 1
            . exp $ a * (max_brightness - fromIntegral mag) - d2 / (2 * w^(2 :: Int))
        in toPixelRGB $ PixelHSI hue (saturation * sat) val
    in liftPx (min 1) . foldl' (liftPx2 (+)) (PixelRGB 0 0 0) $ renderPixel <$> stars
