{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric #-}

module Animation ( Keyframe(camera, time)
                 , Animation(scene, nFrames, interpolation, keyframes)
                 , InterpolationMethod(Linear)
                 , generateFrames, validateKeyframes ) where

import Data.List (sortOn)
import qualified ConfigFile as CF
import Data.Aeson.Types
import Linear ((*^))
import GHC.Generics

data Keyframe = Keyframe { camera :: CF.Camera
                         , time :: Float }
                         deriving (Generic)

data InterpolationMethod = Linear

data Animation = Animation { scene :: CF.Scene
                           , nFrames :: Int
                           , interpolation :: InterpolationMethod
                           , keyframes :: [Keyframe] }
                           deriving (Generic)

instance FromJSON Keyframe

instance FromJSON InterpolationMethod where
    parseJSON str = do
        (str' :: String) <- parseJSON str
        return $ case str' of
            "linear" -> Linear
            _        -> Linear

instance FromJSON Animation

validateKeyframes :: [Keyframe] -> Either String ()
validateKeyframes []  = Left "Must have at least two keyframes"
validateKeyframes [_] = validateKeyframes []
validateKeyframes frs = if time (head frs) == 0 && time (last frs) == 1
    then Right ()
    else Left "First keyframe must have time == 0, last time == 1"

generateFrames :: Animation -> [CF.Config]
generateFrames animation = let
    stepsize = (1 :: Float) / fromIntegral (nFrames animation - 1)
    -- Take the first keyframe from the scene in the config
    -- Also sort the frames by time
    frames = sortOn time $ keyframes animation
    points = (* stepsize) . fromIntegral <$> [0 .. nFrames animation - 1]
    in map (makeFrame animation frames) points

makeFrame :: Animation -> [Keyframe] -> Float -> CF.Config
makeFrame animation frames point = let
        scn = scene animation
        mtd = interpolation animation
    in CF.Config { CF.camera = interpolate mtd frames point
                 , CF.scene = scn }

interpolate :: InterpolationMethod -> [Keyframe] -> Float -> CF.Camera
interpolate method frames t = let
        findFrames (fr1 : fr2 : frs) = if t >= time fr1 && t < time fr2
            then (fr1, fr2)
            else findFrames (fr2 : frs)
        findFrames [fr] = (fr, fr { time = time fr + 1 } )

        (f1, f2) = findFrames frames
        t' = (t - time f1) / (time f2 - time f1)

        f :: Fractional a => (Float -> a -> a) -> a -> a -> a
        f = interpolationFunction method t'

        cam1 = camera f1
        cam2 = camera f2
    in CF.Camera { CF.fov = f (*) (CF.fov cam1) (CF.fov cam2)
                 , CF.position = f (*^) (CF.position cam1) (CF.position cam2)
                 , CF.lookAt = f (*^) (CF.lookAt cam1) (CF.lookAt cam2)
                 , CF.upVec = f (*^) (CF.upVec cam1) (CF.upVec cam2) }

interpolationFunction :: Fractional a => InterpolationMethod -> Float
                                      -> (Float -> a -> a)
                                      -> a -> a -> a
{-# INLINE interpolationFunction #-}
interpolationFunction method t times a b = case method of
    Linear -> a + t `times` (b - a)
