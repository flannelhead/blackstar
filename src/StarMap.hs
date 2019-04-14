{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module StarMap
    ( Star, StarGrid(..), Range, SearchIndex
    , readMapFromFile
    , makeMapFromPPM
    , treeToByteString
    , assembleStarGrid
    , readTreeFromFile
    ) where

import Control.Monad
import Control.DeepSeq
import Data.Array.Accelerate as A (DIM3, Z(..), (:.)(..), Elt, fromList, toList, Vector)
import Data.Array.Accelerate.Linear.V3()
import qualified Data.ByteString as B
import Data.Char
import qualified Data.IntMap.Strict as IM
import Data.Serialize as S
import Data.List as LI
import GHC.Generics
import Linear

import Util

type Star = (V3 Float, Float, Float, Float)
-- (start index, length)
type Range = (Int, Int)
type SearchIndex = Vector Range
-- Division per dimension, search index, vector of stars
data StarGrid = StarGrid Int SearchIndex (Vector Star)
    deriving (Generic, NFData, Serialize)

instance (Serialize a, Elt a) => Serialize (Vector a) where
    put = put . A.toList
    get = fmap fromFiniteList get

fromFiniteList :: (Elt a) => [a] -> Vector a
fromFiniteList lst = A.fromList (Z :. length lst) lst

mapVector :: (Elt a, Elt b) => (a -> b) -> Vector a -> Vector b
mapVector f vec = fromFiniteList . map f $ A.toList vec

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
        max_brightness = 950 -- the "maximum brightness" magnitude
        dynamic = 50         -- "dynamic range": magnitude change that doubles intensity
        a = log 2 / dynamic
        in (x, a * (max_brightness - mag), y, z)
    in StarGrid division idx (mapVector f stars)

readMapFromFile :: FilePath -> IO (Either String [Star])
readMapFromFile path = do
    ebs <- readSafe path
    return $ ebs >>= runGet readMap

makeMapFromPPM :: FilePath -> IO (Either String StarGrid)
makeMapFromPPM path = do
    ppm <- readMapFromFile path
    return $ ppm >>= return . assembleStarGrid 10

readTreeFromFile :: FilePath -> IO (Either String StarGrid)
readTreeFromFile path = do
    ebs <- readSafe path
    return . fmap convertMagnitudes $ S.decode =<< ebs

treeToByteString :: StarGrid -> B.ByteString
treeToByteString = S.encode

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

threeToLinear :: DIM3 -> DIM3 -> Int
threeToLinear (Z :. _ :. sy :. sx) (Z :. z :. y :. x)
    = x + sx * (y + sy * z)

clamp_ :: Ord a => a -> a -> a -> a
clamp_ a b x = max a $ min b x

vecToIndex :: Int -> V3 Float -> DIM3
vecToIndex division vec = let
    vecNormalized = vec ^+^ V3 1 1 1
    V3 x y z = fmap (clamp_ 0 (division - 1) . floor) $ 0.5 * fromIntegral division *^ vecNormalized
    in Z :. z :. y :. x

makeGridShape :: Int -> DIM3
makeGridShape division = Z :. division :. division :. division

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

neighbourCells :: DIM3 -> DIM3 -> [DIM3]
neighbourCells (Z :. zMax :. yMax :. xMax) (Z :. z :. y :. x) = let
    cells = [ Z :. z + i :. y + j :. x + k
            | i <- [-1, 0, 1]
            , j <- [-1, 0, 1]
            , k <- [-1, 0, 1]
            ]

    isWithinBounds (Z :. z' :. y' :. x') =
        z' >= 0 && z' < zMax &&
        y' >= 0 && y' < yMax &&
        x' >= 0 && x' < xMax

    fix cell = if isWithinBounds cell
                  then cell
                  else Z :. 0 :. 0 :. 0

    in map fix cells

assembleStarGrid :: Int -> [Star] -> StarGrid
assembleStarGrid division stars = let
    gridShape = makeGridShape division
    toLinear = threeToLinear gridShape

    indexOfStar = toLinear . vecToIndex division . normalize . fst4
    sortedStars = sortOn indexOfStar stars
    spatialIndices = map indexOfStar sortedStars

    ranges = let
        f [] = []
        f (h : t) = let
            (idx, idx') = h
            (s1, s2) = span ((idx' ==) . snd) t
            in (idx', (idx, length s1 + 1)) : f s2
        in IM.fromAscList . f $ zip [(0 :: Int) ..] spatialIndices
    
    allIndices = [ Z :. i :. j :. k
                 | i <- [0 .. division - 1]
                 , j <- [0 .. division - 1]
                 , k <- [0 .. division - 1]
                 ]
    searchIndex = concatMap
        (\j -> map (\k -> IM.findWithDefault (0, 0) (toLinear k) ranges)
                   $ neighbourCells gridShape j)
        allIndices

    in StarGrid division
         (fromFiniteList searchIndex)
         (fromFiniteList sortedStars)