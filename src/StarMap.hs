{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}

module StarMap
    ( Star, StarTree, StoredStarTree
    , readMapFromFile, readTreeFromFile, treeToByteString, convertTree
    , buildStarTree, sqrnorm, starLookup ) where

import System.Directory
import Control.Monad
import Data.Word
import Data.Char
import qualified Data.ByteString as B
import Data.Serialize
import Data.KdMap.Static
import Linear

import Color

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
    get = skip 1 >> return (defaultSqrDist v3AsList)

instance Serialize (PointAsListFn Double (V3 Double)) where
    put _ = put (0 :: Word8)
    get = skip 1 >> return v3AsList

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
starColor' (!mag, !ch) = let (!h, !s) = starColor ch in (mag, h, s)

-- Some nice colour values for different spectral types
starColor :: Char -> (Double, Double)
starColor 'O' = (227, 0.39)
starColor 'B' = (226, 0.33)
starColor 'A' = (224, 0.21)
starColor 'F' = (234, 0.03)
starColor 'G' = (32, 0.09)
starColor 'K' = (34, 0.29)
starColor 'M' = (34, 0.56)
starColor _   = (0, 0)

raDecToCartesian :: Double -> Double -> V3 Double
raDecToCartesian ra dec = V3 (cos dec*cos ra) (cos dec*sin ra) (sin dec)

readSafe :: FilePath -> IO (Either String B.ByteString)
readSafe path = do
    exists <- doesFileExist path
    if exists then Right <$> B.readFile path
              else return . Left $ "Error: file " ++ path
                  ++ " doesn't exist.\n"

readMapFromFile :: FilePath -> IO (Either String [StoredStar])
readMapFromFile path = do
    ebs <- readSafe path
    return $ ebs >>= runGet readMap

readTreeFromFile :: FilePath -> IO (Either String StoredStarTree)
readTreeFromFile path = do
    ebs <- readSafe path
    return $ decode =<< ebs

treeToByteString :: StoredStarTree -> B.ByteString
treeToByteString = encode

buildStarTree :: [StoredStar] -> StoredStarTree
buildStarTree = build v3AsList

convertTree :: StoredStarTree -> StarTree
convertTree = fmap starColor'

v3AsList :: V3 Double -> [Double]
v3AsList (V3 !x !y !z) = [x, y, z]

sqrnorm :: V3 Double -> Double
{-# INLINE sqrnorm #-}
sqrnorm (V3 !x !y !z) = x*x + y*y + z*z

starLookup :: StarTree -> Double -> Double -> V3 Double -> RGBA
{-# INLINE starLookup #-}
starLookup !starmap !intensity !saturation !vel = let
        -- The magnitude value tells about the intensity of the star. The
        -- brighter the star, the smaller the magnitude. These constants are
        -- used for adjusting the dynamics of the rendered celestial sphere.
        -- We need three fixed points to determine the dynamics:
        -- the points m0 and m1 fix the logarithmic scale. m0 is the reference
        -- "minimum" magnitude. When the magnitude reaches m1, the brightness
        -- will be doubled. m2 is required for normalization and corresponds to
        -- the maximal brightness value that will be represented on the screen.
        m0 = 1350 :: Double  -- the "minimum visible" magnitude
        m1 = 1300 :: Double  -- the "double brightness" magnitude
        m2 = 950 :: Double  -- the "maximum brightness" magnitude
        w = 0.0005  -- width parameter of the gaussian function
        r = 0.002  -- star sampling radius

        nvel = normalize vel
        d2 = sqrnorm $ pos ^-^ nvel  -- the distance from the star on the
                                     -- celestial sphere surface
        (pos, (mag, hue, sat)) = nearest starmap nvel
        -- Conversion from the log magnitude scale to linear brightness
        -- and a Gaussian intensity function. This determines the apparent size
        -- and brightness of the star.
        a = log 2 / (m0 - m1)
        val = (* intensity) . min 1
              . exp $ a*(m2 - fromIntegral mag) - d2/(2*w^2)
    in if d2 < r*r then addAlpha (hsvToRGB (hue, saturation * sat, val)) 1
                   else (0, 0, 0, 1)
