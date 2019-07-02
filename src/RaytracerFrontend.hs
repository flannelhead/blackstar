{-# LANGUAGE TemplateHaskell #-}

module RaytracerFrontend (render, writeImg) where

import Codec.Picture
import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.IO.Codec.Picture

import ConfigFile
import StarMap
import Raytracer

writeImg :: Image PixelRGB8 -> FilePath -> IO ()
writeImg img path = writePng path img

toArr :: Elt a => a -> Scalar a
toArr x = fromList Z [x]

render :: StarGrid -> Scene -> Camera -> Image PixelRGB8
render stargrid scn cam = let
    StarGrid division searchIndex stars = stargrid
    in convertRGB8 . ImageRGBA8 . imageOfArray
        $ $( runQ blackstarProgram) (toArr division) searchIndex
          stars (toArr scn) (toArr cam)