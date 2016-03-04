{-# LANGUAGE TemplateHaskell, CPP #-}

import Data.KdMap.Static as KDM

import Control.Monad
import Data.List
import Data.Ord
import Data.Point2d
import System.Exit
import Test.QuickCheck

#if MIN_VERSION_QuickCheck(2,7,0)
#else
import Test.QuickCheck.All
#endif

testElements :: [p] -> [(p, Int)]
testElements ps = zip ps [0 ..]

prop_validTree :: Property
prop_validTree =
  forAll (listOf1 arbitrary) $ isValid . build pointAsList2d . testElements

checkElements :: (Ord p, Real a) => PointAsListFn a p -> [p] -> Bool
checkElements pointAsList ps =
  let kdt = build pointAsList $ testElements ps
  in  sort (assocs kdt) == sort (testElements ps)

prop_sameElements :: Property
prop_sameElements = forAll (listOf1 arbitrary) $ checkElements pointAsList2d

checkNumElements :: Real a => PointAsListFn a p -> [p] -> Bool
checkNumElements pointAsList ps =
  let kdm = build pointAsList $ testElements ps
  in  size kdm == length ps

prop_validNumElements :: Property
prop_validNumElements = forAll (listOf1 arbitrary) $ checkNumElements pointAsList2d

nearestLinear :: Real a => KDM.PointAsListFn a p -> [(p, v)] -> p -> (p, v)
nearestLinear pointAsList xs query =
  minimumBy (comparing (KDM.defaultSqrDist pointAsList query . fst)) xs

checkNearestEqualToLinear :: (Eq p, Real a) => KDM.PointAsListFn a p -> ([p], p) -> Bool
checkNearestEqualToLinear pointAsList (ps, query) =
  let kdt = build pointAsList $ testElements ps
  in  nearest kdt query == nearestLinear pointAsList (testElements ps) query

prop_nearestEqualToLinear :: Point2d -> Property
prop_nearestEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkNearestEqualToLinear pointAsList2d (xs, query)

inRadiusLinear :: Real a => KDM.PointAsListFn a p -> [(p, v)] -> p -> a -> [(p, v)]
inRadiusLinear pointAsList xs query radius =
  filter ((<= radius * radius) . defaultSqrDist pointAsList query . fst) xs

checkInRadiusEqualToLinear :: (Ord p, Real a) => KDM.PointAsListFn a p -> a -> ([p], p) -> Bool
checkInRadiusEqualToLinear pointAsList radius (ps, query) =
  let kdt = build pointAsList $ testElements ps
      kdtNear = inRadius kdt radius query
      linearNear = inRadiusLinear pointAsList (testElements ps) query radius
  in  sort kdtNear == sort linearNear

prop_inRadiusEqualToLinear :: Point2d -> Property
prop_inRadiusEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (0.0, 1000.0)) $ \radius ->
    checkInRadiusEqualToLinear pointAsList2d radius (xs, query)

kNearestLinear :: Real a => KDM.PointAsListFn a p -> [(p, v)] -> p -> Int -> [(p, v)]
kNearestLinear pointAsList xs query k =
  take k $ sortBy (comparing (defaultSqrDist pointAsList query . fst)) xs

checkKNearestEqualToLinear :: (Ord p, Real a) => KDM.PointAsListFn a p -> Int -> ([p], p) -> Bool
checkKNearestEqualToLinear pointAsList k (xs, query) =
  let kdt = build pointAsList $ testElements xs
      kdtKNear = kNearest kdt k query
      linearKNear = kNearestLinear pointAsList (testElements xs) query k
  in  kdtKNear == linearKNear

prop_kNearestEqualToLinear :: Point2d -> Property
prop_kNearestEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    forAll (choose (1, length xs)) $ \k ->
      checkKNearestEqualToLinear pointAsList2d k (xs, query)

checkKNearestSorted :: (Eq p, Real a) => KDM.PointAsListFn a p -> ([p], p) -> Bool
checkKNearestSorted _ ([], _) = True
checkKNearestSorted pointAsList (ps, query) =
  let kdt = build pointAsList $ testElements ps
      kNearestDists =
        map (defaultSqrDist pointAsList query . fst) $ kNearest kdt (length ps) query
  in  kNearestDists == sort kNearestDists

prop_kNearestSorted :: Point2d -> Property
prop_kNearestSorted query =
  forAll (listOf1 arbitrary) $ \xs ->
    checkKNearestSorted pointAsList2d (xs, query)

rangeLinear :: Real a => KDM.PointAsListFn a p -> [(p, v)] -> p -> p -> [(p, v)]
rangeLinear pointAsList xs lowers uppers =
  let valInRange a lower upper = lower <= a && a <= upper
      lowersAsList = pointAsList lowers
      uppersAsList = pointAsList uppers
      pointInRange (p, _) =
        and $ zipWith3 valInRange (pointAsList p) lowersAsList uppersAsList
  in  filter pointInRange xs

prop_rangeEqualToLinear :: ([Point2d], Point2d, Point2d) -> Bool
prop_rangeEqualToLinear (xs, lowers, uppers)
  | Data.List.null xs = True
  | and $ zipWith (<) (pointAsList2d lowers) (pointAsList2d uppers) =
      let linear = rangeLinear pointAsList2d (testElements xs) lowers uppers
          kdt    = build pointAsList2d $ testElements xs
          kdtPoints = inRange kdt lowers uppers
      in  sort linear == sort kdtPoints
  | otherwise = True

prop_equalAxisValueSameElems :: Property
prop_equalAxisValueSameElems =
  forAll (listOf1 arbitrary) $ \xs@(Point2d x y : _) ->
    checkElements pointAsList2d $ Point2d x (y + 1) : xs

prop_equalAxisValueEqualToLinear :: Point2d -> Property
prop_equalAxisValueEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs@(Point2d x y : _) ->
    checkNearestEqualToLinear pointAsList2d (Point2d x (y + 1) : xs, query)

prop_unbalancedInsertValid :: Property
prop_unbalancedInsertValid =
  forAll (listOf1 arbitrary) $
    isValid . batchInsertUnbalanced (empty pointAsList2d) . testElements

prop_unbalancedInsertNNEqualToLinear :: Point2d -> Property
prop_unbalancedInsertNNEqualToLinear query =
  forAll (listOf1 arbitrary) $ \xs ->
    let kdm = batchInsertUnbalanced (empty pointAsList2d) $ testElements xs
    in  nearest kdm query == nearestLinear pointAsList2d (testElements xs) query

-- Run all tests
return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  success <- runTests
  unless success exitFailure
