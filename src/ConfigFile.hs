{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ConfigFile
    ( Scene( Scene, stopThreshold, stepSize, bloomStrength, bloomDivider
           , starIntensity, starSaturation, supersampling
           , diskColor, diskOpacity, diskInner, diskOuter, resolution )
    , pattern Scene_, stopThreshold_, stepSize_, bloomStrength_
    , bloomDivider_, starIntensity_, starSaturation_, diskColor_
    , diskOpacity_, diskInner_, diskOuter_, resolution_, supersampling_
    , Camera( Camera, position, lookAt, upVec, fov )
    , pattern Camera_, position_, lookAt_, upVec_, fov_
    , Config( Config, camera, scene ) ) where

import Data.Aeson.Types
import Linear
import GHC.Generics
import Data.Array.Accelerate (Elt, Exp, IsTuple, pattern Pattern)
import Data.Array.Accelerate.Linear.V3()
import Data.Array.Accelerate.Data.Colour.HSL

data Config = Config { scene :: Scene
                     , camera :: Camera }
                     deriving (Generic)

data Scene = Scene { stopThreshold :: Float
                   , stepSize :: Float
                   , bloomStrength :: Float
                   , bloomDivider :: Int
                   , starIntensity :: Float
                   , starSaturation :: Float
                   , diskColor :: HSL Float
                   , diskOpacity :: Float
                   , diskInner :: Float
                   , diskOuter :: Float
                   , resolution :: (Int, Int)
                   , supersampling :: Bool }
                   deriving (Generic, Elt, Show, IsTuple)

pattern Scene_ :: Exp Float -> Exp Float -> Exp Float
                  -> Exp Int -> Exp Float -> Exp Float
                  -> Exp (HSL Float) -> Exp Float -> Exp Float
                  -> Exp Float -> Exp (Int, Int) -> Exp Bool
                  -> Exp Scene
pattern Scene_ { stopThreshold_
               , stepSize_
               , bloomStrength_
               , bloomDivider_
               , starIntensity_
               , starSaturation_
               , diskColor_
               , diskOpacity_
               , diskInner_
               , diskOuter_
               , resolution_
               , supersampling_
               } = Pattern
               ( stopThreshold_
               , stepSize_
               , bloomStrength_
               , bloomDivider_
               , starIntensity_
               , starSaturation_
               , diskColor_
               , diskOpacity_
               , diskInner_
               , diskOuter_
               , resolution_
               , supersampling_ )

data Camera = Camera { position :: V3 Float
                     , lookAt :: V3 Float
                     , upVec :: V3 Float
                     , fov :: Float }
                     deriving (Generic, Elt, Show, IsTuple)

pattern Camera_ :: Exp (V3 Float) -> Exp (V3 Float)
                   -> Exp (V3 Float) -> Exp Float
                   -> Exp Camera
pattern Camera_ { position_, lookAt_, upVec_, fov_ } = Pattern (position_, lookAt_, upVec_, fov_)

instance FromJSON (V3 Float) where
    parseJSON vec = do
        [x, y, z] <- parseJSON vec
        return $ V3 x y z

instance ToJSON (V3 Float) where
    toJSON (V3 x y z) = toJSON [x, y, z]

instance FromJSON (HSL Float) where
    parseJSON hsi = do
        [h, s, l] <- parseJSON hsi
        return $ HSL h s l

instance ToJSON (HSL Float) where
    toJSON (HSL h s l) = toJSON [h, s, l]

instance FromJSON Config

instance ToJSON Config where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Camera

instance ToJSON Camera where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Scene where
    parseJSON (Object v) = Scene 0.95                     <$>
                           v .:? "stepSize"       .!= 0.3 <*>
                           v .:? "bloomStrength"  .!= 0.4 <*>
                           v .:? "bloomDivider"   .!= 25  <*>
                           v .:? "starIntensity"  .!= 0.7 <*>
                           v .:? "starSaturation" .!= 0.7 <*>
                           v .:? "diskColor"
                             .!= HSL 58 0.1 0.95   <*>
                           v .:? "diskOpacity"    .!= 0   <*>
                           v .:? "diskInner"      .!= 3   <*>
                           v .:? "diskOuter"      .!= 12  <*>
                           v .:? "resolution"     .!= (1280, 720) <*>
                           v .:? "supersampling"  .!= False

    parseJSON invalid = typeMismatch "Object" invalid

instance ToJSON Scene where
    toEncoding = genericToEncoding defaultOptions
