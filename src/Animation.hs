{-# LANGUAGE OverloadedStrings #-}

module Animation where

import ConfigFile
import Data.Aeson.Types

data Keyframe = Keyframe { camera :: Camera
                         , time :: Double }

data Animation = Animation { scene :: Scene
                           , nFrames :: Int
                           , interpolation :: String
                           , keyframes :: [Keyframe] }

instance FromJSON Keyframe where
    parseJSON (Object v) = Keyframe      <$>
                           v .: "camera" <*>
                           v .: "time"

    parseJSON invalid = typeMismatch "Keyframe" invalid

instance FromJSON Animation where
    parseJSON (Object v) = Animation            <$>
                           v .: "scene"         <*>
                           v .: "nFrames"       <*>
                           v .: "interpolation" <*>
                           v .: "keyframes"

    parseJSON invalid = typeMismatch "Animation" invalid
