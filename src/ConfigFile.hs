{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ConfigFile
    ( Scene( Scene, safeDistance, stepSize, bloomStrength, bloomDivider
           , starIntensity, starSaturation, supersampling
           , diskColor, diskOpacity, diskInner, diskOuter, resolution )
    , Camera( Camera, position, lookAt, upVec, fov )
    , Config( Config, camera, scene ) ) where

import Data.Aeson.Types
import Linear
import GHC.Generics
import Graphics.Image as I

data Config = Config { scene :: Scene
                     , camera :: Camera }
                     deriving (Generic)

data Scene = Scene { safeDistance :: !Double
                   , stepSize :: !Double
                   , bloomStrength :: !Double
                   , bloomDivider :: !Int
                   , starIntensity :: !Double
                   , starSaturation :: !Double
                   , diskColor :: !(Pixel HSI Double)
                   , diskOpacity :: !Double
                   , diskInner :: !Double
                   , diskOuter :: !Double
                   , resolution :: !(Int, Int)
                   , supersampling :: !Bool }
                   deriving (Generic)

data Camera = Camera { position :: !(V3 Double)
                     , lookAt :: !(V3 Double)
                     , upVec :: !(V3 Double)
                     , fov :: !Double }
                     deriving (Generic)

instance FromJSON (V3 Double) where
    parseJSON vec = do
        [x, y, z] <- parseJSON vec
        return $ V3 x y z

instance ToJSON (V3 Double) where
    toJSON (V3 x y z) = toJSON [x, y, z]

instance FromJSON (Pixel HSI Double) where
    parseJSON hsi = do
        [x, y, z] <- parseJSON hsi
        return $ PixelHSI (x / 360) y z

instance ToJSON (Pixel HSI Double) where
    toJSON (PixelHSI h s i) = toJSON [360 * h, s, i]

instance FromJSON Config

instance ToJSON Config where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Camera

instance ToJSON Camera where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Scene where
    parseJSON (Object v) = Scene 0                        <$>
                           v .:? "stepSize"       .!= 0.3 <*>
                           v .:? "bloomStrength"  .!= 0.4 <*>
                           v .:? "bloomDivider"   .!= 25  <*>
                           v .:? "starIntensity"  .!= 0.7 <*>
                           v .:? "starSaturation" .!= 0.7 <*>
                           v .:? "diskColor"
                             .!= PixelHSI 0.16 0.1 0.95   <*>
                           v .:? "diskOpacity"    .!= 0   <*>
                           v .:? "diskInner"      .!= 3   <*>
                           v .:? "diskOuter"      .!= 12  <*>
                           v .:? "resolution"     .!= (1280, 720) <*>
                           v .:? "supersampling"  .!= False

    parseJSON invalid = typeMismatch "Object" invalid

instance ToJSON Scene where
    toEncoding = genericToEncoding defaultOptions
