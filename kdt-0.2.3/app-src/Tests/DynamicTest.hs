{-# LANGUAGE TemplateHaskell, CPP #-}

import qualified Data.KdMap.Static as KDM
import Data.KdMap.Dynamic

import Control.Monad
import Data.Bits
import Data.List
import Data.Point2d
import System.Exit
import Test.QuickCheck

#if MIN_VERSION_QuickCheck(2,7,0)
#else
import Test.QuickCheck.All
#endif

testElements :: [p] -> [(p, Int)]
testElements ps = zip ps [1 ..]

checkLogNTrees :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> [p] -> Bool
checkLogNTrees p2l d2 ps =
  let lengthIsLogN kdm = length (subtreeSizes kdm) == popCount (size kdm)
  in  all lengthIsLogN $ scanl insertPair (emptyWithDist p2l d2) $ testElements ps

prop_logNTrees :: [Point2d] -> Bool
prop_logNTrees = checkLogNTrees pointAsList2d distSqr2d

checkTreeSizesPowerOf2 :: Real a => PointAsListFn a p ->
                                    SquaredDistanceFn a p ->
                                    [p] ->
                                    Bool
checkTreeSizesPowerOf2 p2l d2 ps =
  let sizesPowerOf2 = all ((== 1) . popCount) . subtreeSizes
  in  all sizesPowerOf2 $ scanl insertPair (emptyWithDist p2l d2) $ testElements ps

prop_treeSizesPowerOf2 :: [Point2d] -> Bool
prop_treeSizesPowerOf2 = checkTreeSizesPowerOf2 pointAsList2d distSqr2d

checkNumElements :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> [p] -> Bool
checkNumElements p2l d2 ps =
  let numsMatch (num, kdm) = size kdm == num && num == sum (subtreeSizes kdm)
  in  all numsMatch $ zip [0..] $ scanl insertPair (emptyWithDist p2l d2) $ testElements ps

prop_validNumElements :: [Point2d] -> Bool
prop_validNumElements = checkNumElements pointAsList2d distSqr2d

checkNearestEqualToBatch :: (Eq p, Real a) => PointAsListFn a p ->
                                              SquaredDistanceFn a p ->
                                              ([p], p) ->
                                              Bool
checkNearestEqualToBatch p2l d2 (ps, query) =
  let kdt = KDM.buildWithDist p2l d2 $ testElements ps
      kdtAnswer = KDM.nearest kdt query
      dkdt = batchInsert (emptyWithDist p2l d2) $ testElements ps
      dkdtAnswer = nearest dkdt query
  in  dkdtAnswer == kdtAnswer

prop_nearestEqualToBatch :: Point2d -> Property
prop_nearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestEqualToBatch pointAsList2d distSqr2d (xs, query)

checkKNearestEqualToBatch :: (Eq p, Real a) => PointAsListFn a p ->
                                               SquaredDistanceFn a p ->
                                               ([p], Int, p) ->
                                               Bool
checkKNearestEqualToBatch p2l d2 (ps, k, query) =
  let kdt = KDM.buildWithDist p2l d2 $ testElements ps
      kdtAnswer = KDM.kNearest kdt k query
      dkdt = batchInsert (emptyWithDist p2l d2) $ testElements ps
      dkdtAnswer = kNearest dkdt k query
  in  dkdtAnswer == kdtAnswer

prop_kNearestEqualToBatch :: Point2d -> Property
prop_kNearestEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (1, length xs)) $ \k ->
      checkKNearestEqualToBatch pointAsList2d distSqr2d (xs, k, query)

checkInRadiusEqualToBatch :: (Ord p, Real a) => PointAsListFn a p ->
                                            SquaredDistanceFn a p ->
                                            ([p], a, p) ->
                                            Bool
checkInRadiusEqualToBatch p2l d2 (ps, radius, query) =
  let kdt = KDM.buildWithDist p2l d2 $ testElements ps
      kdtAnswer = KDM.inRadius kdt radius query
      dkdt = batchInsert (emptyWithDist p2l d2) $ testElements ps
      dkdtAnswer = inRadius dkdt radius query
  in  sort dkdtAnswer == sort kdtAnswer

prop_checkInRadiusEqualToBatch :: Point2d -> Property
prop_checkInRadiusEqualToBatch query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (0.0, 1000.0)) $ \radius ->
      checkInRadiusEqualToBatch pointAsList2d distSqr2d (xs, radius, query)

prop_checkInRangeEqualToBatch :: ([Point2d], Point2d, Point2d) -> Bool
prop_checkInRangeEqualToBatch ([], _, _) = True
prop_checkInRangeEqualToBatch (xs, lowers, uppers)
  | and $ zipWith (<) (pointAsList2d lowers) (pointAsList2d uppers) =
      let kdt = KDM.buildWithDist pointAsList2d distSqr2d $ testElements xs
          kdtAnswer = KDM.inRange kdt lowers uppers
          dkdt = batchInsert (emptyWithDist pointAsList2d distSqr2d) $ testElements xs
          dkdtAnswer = inRange dkdt lowers uppers
      in  sort dkdtAnswer == sort kdtAnswer
  | otherwise = True


-- Run all tests
return []
runTests :: IO Bool
runTests =  $quickCheckAll

main :: IO ()
main = do
  success <- runTests
  unless success exitFailure
