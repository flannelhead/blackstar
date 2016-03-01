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

type Star = (V3 Double, (Int, Word8, Word8))

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

buildStarTree :: [Star] -> KdMap Double (V3 Double) (Int, Word8, Word8)
buildStarTree stars = build (\(V3 !x !y !z) -> [x, y, z]) stars

sqrnorm :: V3 Double -> Double
{-# INLINE sqrnorm #-}
sqrnorm (V3 !x !y !z) = x*x + y*y + z*z

starLookup :: KdMap Double (V3 Double) (Int, Word8, Word8) -> V3 Double
              -> I.RGBPixel
{-# INLINE starLookup #-}
starLookup !starmap !vel = pxl
    where (pos, (mag, hue, sat)) = nearest starmap $ normalize vel
          d2 = sqrnorm $ pos ^-^ vel
          pxl = if d2 < r2 then I.convert $ I.HSVPixel hue (sat `div` 2) val
                           else I.RGBPixel 0 0 0

          val = floor . max 0 . min 255 . (* minVal)
                . exp $ a*(m0 - fromIntegral mag) - d2/w**2
          minVal = 1
          maxVal = 255
          a = log (maxVal / minVal) / (m0 - m1)
          m0 = 1350 :: Double
          m1 = 950 :: Double
          w = 0.001
          r2 = 1
