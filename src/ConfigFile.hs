{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module ConfigFile
    ( Scene( Scene, safeDistance, stepSize, camera, bloomStrength, bloomDivider
           , starIntensity, starSaturation, supersampling
           , diskColor, diskOpacity, diskInner, diskOuter )
    , Camera( Camera, position, lookAt, upVec, fov, resolution ) ) where

import Data.Yaml
import Data.Aeson.Types
import Linear

import Color (RGB(RGB), HSV(HSV), hsvToRGB)

data Scene = Scene { safeDistance :: !Double
                   , stepSize :: !Double
                   , camera :: !Camera
                   , bloomStrength :: !Double
                   , bloomDivider :: !Int
                   , starIntensity :: !Double
                   , starSaturation :: !Double
                   , diskColor :: !RGB
                   , diskOpacity :: !Double
                   , diskInner :: !Double
                   , diskOuter :: !Double
                   , supersampling :: !Bool }

data Camera = Camera { position :: !(V3 Double)
                     , lookAt :: !(V3 Double)
                     , upVec :: !(V3 Double)
                     , fov :: !Double
                     , resolution :: !(Int, Int) }

instance FromJSON (V3 Double) where
    parseJSON vec = do
        [x, y, z] <- parseJSON vec
        return $ V3 x y z

instance FromJSON RGB where
    parseJSON rgb = do
        [x, y, z] <- parseJSON rgb
        return . hsvToRGB $ HSV x y z

instance FromJSON Camera where
    parseJSON (Object v) = Camera            <$>
                           v .: "position"   <*>
                           v .: "lookAt"     <*>
                           v .: "upVec"      <*>
                           v .: "fov"        <*>
                           v .: "resolution"

    parseJSON invalid = typeMismatch "Camera" invalid

instance FromJSON Scene where
    parseJSON (Object v) = Scene 0                        <$>
                           v .:? "stepSize"       .!= 0.3 <*>
                           v .:  "camera"                 <*>
                           v .:? "bloomStrength"  .!= 0.4 <*>
                           v .:? "bloomDivider"   .!= 25  <*>
                           v .:? "starIntensity"  .!= 0.7 <*>
                           v .:? "starSaturation" .!= 0.7 <*>
                           v .:? "diskHSV"
                             .!= RGB 0.95 0.95 0.85       <*>
                           v .:? "diskOpacity"    .!= 0   <*>
                           v .:? "diskInner"      .!= 3   <*>
                           v .:? "diskOuter"      .!= 12  <*>
                           v .:? "supersampling"  .!= False

    parseJSON invalid = typeMismatch "Object" invalid
