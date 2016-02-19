module Main where

import GR
import Criterion.Main

makeChristoffels :: Int -> String
makeChristoffels l = unlines (map row [0..3]) ++ "\n" where
    row :: Int -> String
    row j = concatMap (cell j) [0..3]

    cell :: Int -> Int -> String
    cell j k = show (christoffel schwarzschild ischwarzschild (l, j, k)
        (FVect (0 :: Double) 2 (pi/2) 0)) ++ "\t"

allChristoffels :: Double -> [Double]
allChristoffels r = [ christoffel schwarzschild ischwarzschild (l, j, k)
    (FVect 0 r (pi/2) 0) | l <- [0..3], j <- [0..3], k <- [0..3] ]

main :: IO ()
main = defaultMain [ bench "christoffels" $ nf allChristoffels 2 ]
--main = putStrLn $ concatMap makeChristoffels [0..3]

