{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module ConfigFile
    ( Scene( Scene, safeDistance, stepSize, camera, bloomStrength
           , starIntensity, starSaturation
           , diskColor, diskOpacity, diskInner, diskOuter )
    , Camera( Camera, position, lookAt, upVec, fov, resolution ) ) where

import Data.Yaml
import Data.Aeson.Types
import Linear

import Color (RGB)

data Scene = Scene { safeDistance :: !Double
                   , stepSize :: !Double
                   , camera :: !Camera
                   , bloomStrength :: !Double
                   , starIntensity :: !Double
                   , starSaturation :: !Double
                   , diskColor :: !RGB
                   , diskOpacity :: !Double
                   , diskInner :: !Double
                   , diskOuter :: !Double }

data Camera = Camera { position :: !(V3 Double)
                     , lookAt :: !(V3 Double)
                     , upVec :: !(V3 Double)
                     , fov :: !Double
                     , resolution :: !(Int, Int) }

instance FromJSON (V3 Double) where
    parseJSON vec = do
        [x, y, z] <- parseJSON vec
        return $ V3 x y z

instance FromJSON Camera where
    parseJSON (Object v) = Camera            <$>
                           v .: "position"   <*>
                           v .: "lookAt"     <*>
                           v .: "upVec"      <*>
                           v .: "fov"        <*>
                           v .: "resolution"

    parseJSON invalid = typeMismatch "Camera" invalid

instance FromJSON Scene where
    parseJSON (Object v) = Scene 0                         <$>
                           v .:? "stepSize"       .!= 0.15 <*>
                           v .:  "camera"                  <*>
                           v .:? "bloomStrength"  .!= 0.4  <*>
                           v .:? "starIntensity"  .!= 0.7  <*>
                           v .:? "starSaturation" .!= 0.7  <*>
                           v .:? "diskHSV"
                             .!= (60, 0.1, 0.95)           <*>
                           v .:? "diskOpacity"    .!= 0    <*>
                           v .:? "diskInner"      .!= 3    <*>
                           v .:? "diskOuter"      .!= 12

    parseJSON invalid = typeMismatch "Object" invalid
