{-# LANGUAGE BangPatterns #-}

module StarMap where

import Control.Monad
import Data.Word
import Data.Char
import Data.ByteString as B hiding (map)
import Data.Serialize.Get
import Data.Serialize.IEEE754
import Data.KdMap.Static
import Linear
import qualified Vision.Image as I

import Color

type Star = (V3 Double, (Int, Word8, Word8))
type StarTree = KdMap Double (V3 Double) (Int, Word8, Word8)

readMap :: Get [Star]
readMap = do
    skip 28
    nBytes <- remaining
    replicateM (nBytes `div` 28) $ do
        ra <- getFloat64be
        dec <- getFloat64be
        spectral <- getWord8
        skip 1
        mag <- getInt16be
        skip 8
        return $ (raDecToCartesian ra dec, starColor' (fromIntegral mag)
                     . chr $ fromIntegral spectral)

starColor' :: Int -> Char -> (Int, Word8, Word8)
{-# INLINE starColor' #-}
starColor' !mag !ch = let (!h, !s) = starColor ch in (mag, h, s)

starColor :: Char -> (Word8, Word8)
{-# INLINE starColor #-}
starColor 'O' = (114, 99)
starColor 'B' = (113, 84)
starColor 'A' = (112, 54)
starColor 'F' = (117, 8)
starColor 'G' = (16, 23)
starColor 'K' = (17, 74)
starColor 'M' = (17, 143)
starColor _   = (0, 0)

raDecToCartesian :: Double -> Double -> V3 Double
raDecToCartesian ra dec = V3 (cos dec*cos ra) (cos dec*sin ra) (sin dec)

readMapFromFile :: FilePath -> IO (Either String [Star])
readMapFromFile path = do
    bs <- B.readFile path
    return $ runGet readMap bs

buildStarTree :: [Star] -> StarTree
buildStarTree stars = build (\(V3 !x !y !z) -> [x, y, z]) stars

sqrnorm :: V3 Double -> Double
{-# INLINE sqrnorm #-}
sqrnorm (V3 !x !y !z) = x*x + y*y + z*z

starLookup :: StarTree -> V3 Double -> Rgba
{-# INLINE starLookup #-}
starLookup !starmap !vel = let
        r = 0.002  -- star sampling radius
        m0 = 1350 :: Double  -- the "minimum visible" magnitude
        m1 = 930 :: Double -- the "saturated"
        w = 0.0005  -- width parameter of the gaussian function
        nvel = normalize vel
        d2 = sqrnorm $ pos ^-^ nvel  -- the distance from the star on the
                                     -- celestial sphere surface
        a = log 255 / (m0 - m1)
        (pos, (mag, hue, sat)) = nearest starmap nvel
        -- Conversion from the log magnitude scale to linear brightness
        -- and a Gaussian intensity function. This determines the apparent size
        -- and brightness of the star.
        val = floor . max 0 . min 255
              . exp $ a*(m0 - fromIntegral mag) - d2/(2*w**2)
    in if d2 < r*r then fromRGBPixel . I.convert $ I.HSVPixel hue
                       (floor $ (0.75 :: Double) * fromIntegral sat) val
                   else Rgba 0 0 0 1
