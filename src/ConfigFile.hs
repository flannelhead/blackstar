{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ConfigFile
    ( Scene( Scene, safeDistance, stepSize, camera, bloomStrength, bloomDivider
           , starIntensity, starSaturation, supersampling
           , diskColor, diskOpacity, diskInner, diskOuter, resolution )
    , Camera( Camera, position, lookAt, upVec, fov ) ) where

import Data.Aeson.Types
import Linear
import GHC.Generics

import Color (HSV(HSV))

data Scene = Scene { safeDistance :: !Double
                   , stepSize :: !Double
                   , camera :: !Camera
                   , bloomStrength :: !Double
                   , bloomDivider :: !Int
                   , starIntensity :: !Double
                   , starSaturation :: !Double
                   , diskColor :: !HSV
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

instance FromJSON HSV where
    parseJSON hsv = do
        [x, y, z] <- parseJSON hsv
        return $ HSV x y z

instance ToJSON HSV where
    toJSON (HSV h s v) = toJSON [h, s, v]

instance FromJSON Camera

instance ToJSON Camera where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Scene where
    parseJSON (Object v) = Scene 0                        <$>
                           v .:? "stepSize"       .!= 0.3 <*>
                           v .:  "camera"                 <*>
                           v .:? "bloomStrength"  .!= 0.4 <*>
                           v .:? "bloomDivider"   .!= 25  <*>
                           v .:? "starIntensity"  .!= 0.7 <*>
                           v .:? "starSaturation" .!= 0.7 <*>
                           v .:? "diskHSV"
                             .!= HSV 60 0.1 0.95          <*>
                           v .:? "diskOpacity"    .!= 0   <*>
                           v .:? "diskInner"      .!= 3   <*>
                           v .:? "diskOuter"      .!= 12  <*>
                           v .:? "resolution"     .!= (1280, 720) <*>
                           v .:? "supersampling"  .!= False

    parseJSON invalid = typeMismatch "Object" invalid

instance ToJSON Scene where
    toEncoding = genericToEncoding defaultOptions
