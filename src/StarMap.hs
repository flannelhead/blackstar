{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module StarMap
    ( Star, StarGrid(..), Range, SearchIndex
    , readMapFromFile
    , gridToByteString
    , assembleStarGrid
    , readGridFromFile
    ) where

import Control.Monad
import Control.DeepSeq
import Data.Int
import Data.Array.Accelerate as A (DIM3, DIM4, Z(..), (:.)(..),
                                   Array, Vector, fromFunction)
import Data.Array.Accelerate.IO.Data.Serialize()
import Data.Array.Accelerate.IO.Data.Vector.Generic()
import Data.Array.Accelerate.Linear.V3()
import qualified Data.ByteString.Lazy as B
import Data.Char
import qualified Data.IntMap.Strict as IM
import Data.Serialize as S
import Data.List
import qualified Data.Vector.Generic as V
import GHC.Generics
import Linear
import Codec.Compression.GZip

import Util

type Star = (V3 Float, Float, Float, Float)
-- (start index, length)
type Range = (Int32, Int32)
type SearchIndex = Array DIM4 Range
-- Division per dimension, search index, vector of stars
data StarGrid = StarGrid Int SearchIndex (Vector Star)
    deriving (Generic, NFData, Serialize)

-- Parse the star list in the binary format specified at
-- http://tdc-www.harvard.edu/software/catalogs/ppm.entry.html
readMap :: Get [Star]
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
        let (h, s) = starColor . chr $ fromIntegral spectral
        return ( raDecToCartesian (realToFrac ra) (realToFrac dec)
               , fromIntegral mag, h * 360, s )

convertMagnitudes :: StarGrid -> StarGrid
convertMagnitudes (StarGrid division idx stars) = let
    f :: Star -> Star
    f (x, mag, y, z) = let
        -- The magnitude determines the intensity of the star. The
        -- brighter the star, the smaller the magnitude. These constants
        -- are used for adjusting the light dynamics of the rendered
        -- celestial sphere.
        maxBrightness = 950 -- the "maximum brightness" magnitude
        dynamic = 50        -- "dynamic range": magnitude change that doubles intensity
        a = log 2 / dynamic
        in (x, a * (maxBrightness - max (maxBrightness - 4 * dynamic) mag), y, z)
    in StarGrid division idx (V.map f stars)

readMapFromFile :: FilePath -> IO (Either String [Star])
readMapFromFile path = do
    ebs <- readSafe path
    return $ ebs >>= runGetLazy readMap

readGridFromFile :: FilePath -> IO (Either String StarGrid)
readGridFromFile path = do
    ebs <- readSafe path
    return . fmap convertMagnitudes $ S.decodeLazy . decompress =<< ebs

gridToByteString :: StarGrid -> B.ByteString
gridToByteString = compress . S.encodeLazy

-- Some nice colour values for different spectral types
starColor :: Char -> (Float, Float)
starColor 'O' = (0.631, 0.39)
starColor 'B' = (0.628, 0.33)
starColor 'A' = (0.622, 0.21)
starColor 'F' = (0.650, 0.03)
starColor 'G' = (0.089, 0.09)
starColor 'K' = (0.094, 0.29)
starColor 'M' = (0.094, 0.56)
starColor _   = (0, 0)

raDecToCartesian :: Float -> Float -> V3 Float
raDecToCartesian ra dec = V3 (cos dec * cos ra) (cos dec * sin ra) (sin dec)

threeToLinear :: DIM3 -> DIM3 -> Int32
threeToLinear (Z :. _ :. sy :. sx) (Z :. z :. y :. x)
    = fromIntegral $ x + sx * (y + sy * z)

clamp_ :: Ord a => a -> a -> a -> a
clamp_ a b = max a . min b

vecToIndex :: Int -> V3 Float -> DIM3
vecToIndex division vec = let
    vecNormalized = normalize vec ^+^ V3 1 1 1
    V3 x y z = fmap (clamp_ 0 (division - 1) . floor)
                 $ 0.5 * fromIntegral division *^ vecNormalized
    in Z :. z :. y :. x

makeGridShape :: Int -> DIM3
makeGridShape division = Z :. division :. division :. division

cellOffsets :: [DIM3]
cellOffsets =
    [ Z :. i :. j :. k
    | i <- [-1, 0, 1]
    , j <- [-1, 0, 1]
    , k <- [-1, 0, 1]
    ]

clampToBoundary :: DIM3 -> DIM3 -> DIM3
clampToBoundary (Z :. zMax :. yMax :. xMax) cell = let
    isWithinBounds (Z :. z' :. y' :. x') =
        z' >= 0 && z' < zMax &&
        y' >= 0 && y' < yMax &&
        x' >= 0 && x' < xMax
    in if isWithinBounds cell
       then cell
       else Z :. 0 :. 0 :. 0

addIndices :: DIM3 -> DIM3 -> DIM3
addIndices (Z :. i :. j :. k) (Z :. x :. y :. z) =
    Z :. i + x :. j + y :. k + z

neighbourCell :: DIM3 -> DIM4 -> Int32
neighbourCell dims (cell :. subIdx) =
    threeToLinear dims . clampToBoundary dims . addIndices cell
    $ cellOffsets !! subIdx

findRanges :: [(Int32, Int32)] -> [(Int, (Int32, Int32))]
findRanges [] = []
findRanges (h : t) = let
    (idx, idx') = h
    (s1, s2) = span ((idx' ==) . snd) t
    in (fromIntegral idx', (idx, fromIntegral $ length s1 + 1 :: Int32)) : findRanges s2

assembleStarGrid :: Int -> [Star] -> StarGrid
assembleStarGrid division stars = let
    gridShape = makeGridShape division
    toLinear = threeToLinear gridShape
    indexOfStar = toLinear . vecToIndex division . (\(x, _, _, _) -> x)
    sortedStars = sortOn indexOfStar stars
    spatialIndices = map indexOfStar sortedStars

    ranges = IM.fromAscList . findRanges $ zip [0..] spatialIndices
    generate = (\i -> IM.findWithDefault (0, 0) (fromIntegral i) ranges)
               . neighbourCell gridShape

    in StarGrid division
         (fromFunction (gridShape :. 27) generate)
         (V.fromList sortedStars)