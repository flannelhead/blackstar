{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}

module Raytracer (render, writeImg) where

import Linear hiding (lookAt, mult, trace)
import qualified Linear as L
import Control.Lens
import Data.List (foldl', scanl')
import Graphics.Image as I
import Graphics.Image.Interface
import Prelude as P

import StarMap
import ConfigFile
import ImageFilters

data Layer = Layer (Pixel RGBA Double) | Bottom (Pixel RGBA Double) | None
data PhotonState = PhotonState (V3 Double) (V3 Double)

writeImg :: Image VU RGB Double -> FilePath -> IO ()
writeImg img path =
    writeImageExact PNG [] path . exchange VS . compute . toWord8I $ img

blend :: Pixel RGBA Double -> Pixel RGBA Double -> Pixel RGBA Double
blend (PixelRGBA tr tg tb ta) (PixelRGBA br bg bb ba) = let
        a = ta + ba * (1 - ta)
        comp tc bc = if a == 0 then 0 else (tc*ta + bc*ba*(1-ta)) / a
     in PixelRGBA (comp tr br) (comp tg bg) (comp tb bb) a

-- Generate the sight rays ie. initial conditions for the integration
generateRay :: Config -> (Int, Int) -> PhotonState
generateRay cfg (y', x') = PhotonState vel pos
    where cam = camera cfg
          pos = position cam
          scn = scene cfg
          w = fromIntegral . fst $ resolution scn
          h = fromIntegral . snd $ resolution scn
          matr = L.lookAt pos (lookAt cam) (upVec cam) ^. _m33
          vel  = L.normalize . (L.transpose matr !*)
                 $ V3 (fov cam * (fromIntegral x' / w - 0.5))
                      (fov cam * (0.5 - fromIntegral y' / h) * h/w)
                      (-1)

render :: Config -> StarTree -> Image VU RGB Double
render cfg startree = let
    scn = scene cfg
    cam = camera cfg
    (w, h) = resolution scn
    res@(w', h') = if supersampling scn then (2*w, 2*h) else (w, h)
    scn' = scn { safeDistance =
                   max (50^(2 :: Int)) (2 * quadrance (position cam))
               , diskInner = diskInner scn ^ (2 :: Int)
               , diskOuter = diskOuter scn ^ (2 :: Int)
               , resolution = res }
    cfg' = cfg { scene = scn' }
    diskRGB = toPixelRGB $ diskColor scn
    img = makeImage (h', w') $ traceRay cfg' diskRGB startree :: Image RPU RGB Double
    final = toManifest img :: Image VU RGB Double
    in if supersampling scn then supersample final else final

traceRay :: Config -> Pixel RGB Double -> StarTree -> (Int, Int)
            -> Pixel RGB Double
traceRay cfg diskRGB startree pt = let
        ray@(PhotonState vel pos) = generateRay cfg pt
        h2 = quadrance $ pos `cross` vel
        scn = scene cfg
    in dropAlpha . colorize scn diskRGB startree h2 $ ray

colorize :: Scene -> Pixel RGB Double -> StarTree -> Double -> PhotonState
            -> Pixel RGBA Double
colorize scn diskRGB startree h2 crd = let
    colorize' rgba crd' = let
        newCrd = rk4 (stepSize scn) h2 crd'
        in case findColor scn diskRGB startree crd' newCrd of
            Layer rgba' -> colorize' (blend rgba rgba') newCrd
            Bottom rgba' -> blend rgba rgba'
            None -> colorize' rgba newCrd
    in colorize' (PixelRGBA 0 0 0 0) crd

findColor :: Scene -> Pixel RGB Double -> StarTree -> PhotonState -> PhotonState
             -> Layer
{-# INLINE findColor #-}
findColor scn diskRGB startree (PhotonState vel pos@(V3 _ y _))
    (PhotonState _ newPos@(V3 _ y' _))
    | r2 < 1 = Bottom (PixelRGBA 0 0 0 1)  -- already passed the event horizon
    | r2 > safeDistance scn = Bottom . addAlpha 1.0
        $ starLookup startree (starIntensity scn) (starSaturation scn) vel
    | diskOpacity scn /= 0 && signum y' /= signum y
        && r2ave > diskInner scn && r2ave < diskOuter scn
        = Layer $ diskColor' scn diskRGB (sqrt r2ave)
    | otherwise = None
    where r2 = quadrance pos
          r2' = quadrance newPos
          r2ave = (y'*r2 - y*r2') / (y' - y)

diskColor' :: Scene -> Pixel RGB Double -> Double -> Pixel RGBA Double
{-# INLINE diskColor' #-}
diskColor' scn diskRGB r = let
        rInner = sqrt (diskInner scn)
        rOuter = sqrt (diskOuter scn)
        alpha = sin (pi * ((rOuter-r) / (rOuter-rInner))^(2 :: Int))
    in addAlpha (alpha * diskOpacity scn) diskRGB

rk4 :: Double -> Double -> PhotonState -> PhotonState
{-# INLINE rk4 #-}
rk4 h h2 y = let
        mul :: PhotonState -> Double -> PhotonState
        {-# INLINE mul #-}
        mul (PhotonState u v) a = PhotonState (u ^* a) (v ^* a)

        add :: PhotonState -> PhotonState -> PhotonState
        {-# INLINE add #-}
        add (PhotonState x z) (PhotonState u v) = PhotonState (x ^+^ u) (z ^+^ v)

        f :: PhotonState -> PhotonState
        {-# INLINE f #-}
        f (PhotonState vel pos) =
            PhotonState (-1.5*h2 / (norm pos ^ (5 :: Int)) *^ pos) vel

        g :: PhotonState -> Double -> PhotonState
        {-# INLINE g #-}
        g k c = f . add y $ mul k c
    in foldl' add y $ P.zipWith mul (scanl' g (f y) [h/2, h/2, h])
           [(h/6), (h/3), (h/3), (h/6)]
