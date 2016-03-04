{-# LANGUAGE FlexibleContexts #-}

import Data.Point2d
import Data.KdTree.Static as KDT
import Data.KdTree.Dynamic as DKDT

import Control.DeepSeq
import Control.Monad
import qualified Control.Monad.Random as CMR
import Criterion.Main
import Data.List
import Data.Maybe
import qualified Data.Heap as Q
import System.Random.Mersenne.Pure64

zeroOnePointSampler :: CMR.Rand PureMT Point2d
zeroOnePointSampler =
  liftM2 Point2d
    (CMR.getRandomR (0.0, 1.0))
    (CMR.getRandomR (0.0, 1.0))

-- Input: List of pairs of points, where first of each pair is the
-- point to add to the dynamic KdTree, and the second is the point to
-- query for nearest neighbor
interleaveBuildQuery :: [(Point2d, Point2d)] -> [Point2d]
interleaveBuildQuery =
  let f :: (DKDT.KdTree Double Point2d, [Point2d]) ->
           (Point2d, Point2d) ->
           (DKDT.KdTree Double Point2d, [Point2d])
      f (kdt, accList) (treePt, queryPt) =
        let newKdt = DKDT.insert kdt treePt
            near = DKDT.nearest newKdt queryPt
        in  (newKdt, near : accList)
      start = (DKDT.emptyWithDist pointAsList2d distSqr2d, [])
  in  snd . foldl' f start

-- nn implemented with optimized linear scan
nearestLinear :: [Point2d] -> Point2d -> Point2d
nearestLinear [] _ = error "nearestLinear called on an empty list!"
nearestLinear (ph : pt) query = fst $ foldl' f (ph, distSqr2d query ph) pt
  where {-# INLINE f #-}
        f b@(_, dBest) x
          | d < dBest = (x, d)
          | otherwise = b
          where d = distSqr2d query x

pointsInRadiusLinear :: [Point2d] -> Double -> Point2d -> [Point2d]
pointsInRadiusLinear ps radius query =
  filter ((<= radius * radius) . distSqr2d query) ps

-- knn implemented with priority queue
kNearestNeighborsLinear :: [Point2d] -> Int -> Point2d -> [Point2d]
kNearestNeighborsLinear ps k query =
  reverse $ map snd $ Q.toAscList $ foldl' f (Q.empty :: Q.MaxPrioHeap Double Point2d) ps
  where f q p = let insertBounded queue dist x
                      | Q.size queue < k = Q.insert (dist, x) queue
                      | otherwise =
                        let ((farthestDist, _), rest) = fromJust $ Q.view queue
                        in  if dist < farthestDist
                            then Q.insert (dist, x) rest
                            else queue
                in  insertBounded q (distSqr2d query p) p

rangeLinear :: Point2d -> Point2d -> [Point2d] -> [Point2d]
rangeLinear lowers uppers xs =
  let lowersAsList = pointAsList2d lowers
      uppersAsList = pointAsList2d uppers
      valInRange l x u = l <= x && x <= u
      pointInRange p =
        and $ zipWith3 valInRange
          lowersAsList (pointAsList2d p) uppersAsList
  in  filter pointInRange xs

pointToBounds :: Point2d -> Double -> (Point2d, Point2d)
pointToBounds (Point2d x y) w =
  (Point2d (x - w) (y - w), Point2d (x + w) (y + w))

rangeOfPointLinear :: [Point2d] -> Double -> Point2d -> [Point2d]
rangeOfPointLinear xs w q =
  let (lowers, uppers) = pointToBounds q w
  in  rangeLinear lowers uppers xs

rangeOfPointKdt :: KDT.KdTree Double Point2d -> Double -> Point2d -> [Point2d]
rangeOfPointKdt kdt w q =
  let (lowers, uppers) = pointToBounds q w
  in  KDT.inRange kdt lowers uppers

linearInterleaveBuildQuery :: [(Point2d, Point2d)] -> [Point2d]
linearInterleaveBuildQuery =
  let f :: ([Point2d], [Point2d]) -> (Point2d, Point2d) -> ([Point2d], [Point2d])
      f (ps, accList) (structPt, queryPt) =
        let ps' = structPt : ps
            near = nearestLinear ps' queryPt
        in  (ps', near : accList)
  in  snd . foldl' f ([], [])

main :: IO ()
main =
  let seed = 1
      treePoints = CMR.evalRand (sequence $ repeat zeroOnePointSampler) $ pureMT seed
      kdtN n = KDT.buildWithDist pointAsList2d distSqr2d $ take n treePoints
      queryPoints = CMR.evalRand (sequence $ repeat zeroOnePointSampler) $ pureMT (seed + 1)
      buildKdtBench n = bench (show n) $ nf kdtN n
      nnKdtBench nq np =
        bench ("np-" ++ show np ++ "-nq-" ++ show nq) $
          nf (map (KDT.nearest (kdtN np))) (take nq queryPoints)
      inRadKdtBench nq r np =
        bench ("np-" ++ show np ++ "-nq-" ++ show nq ++ "-r-" ++ show r) $
          nf (map (KDT.inRadius (kdtN np) r)) (take nq queryPoints)
      knnKdtBench nq k np =
        bench ("np-" ++ show np ++ "-nq-" ++ show nq ++ "-k-" ++ show k) $
          nf (map (KDT.kNearest (kdtN np) k)) (take nq queryPoints)
      rangeKdtBench nq w np =
        bench ("np-" ++ show np ++ "-nq-" ++ show nq ++ "-w-" ++ show w) $
          nf (map $ rangeOfPointKdt (kdtN np) w) (take nq queryPoints)
      nnLinearBench nq np =
        bench ("np-" ++ show np ++ "-nq-" ++ show nq) $
          nf (map (nearestLinear (take np treePoints))) (take nq queryPoints)
      inRadLinearBench nq r np =
        bench ("np-" ++ show np ++ "-nq-" ++ show nq ++ "-r-" ++ show r) $
          nf (map $ pointsInRadiusLinear (take np treePoints) r) (take nq queryPoints)
      rangeLinearBench nq w np =
        bench ("np-" ++ show np ++ "-nq-" ++ show nq ++ "-w-" ++ show w) $
          nf (map $ rangeOfPointLinear (take np treePoints) w) (take nq queryPoints)
      knnLinearBench nq k np =
        bench ("np-" ++ show np ++ "-nq-" ++ show nq ++ "-k-" ++ show k) $
          nf (map $ kNearestNeighborsLinear (take np treePoints) k) (take nq queryPoints)
      nniDkdtBench n =
        bench ("n-" ++ show n) $
          nf interleaveBuildQuery (zip (take n treePoints) (take n queryPoints))
      numQueries = 100
      pointSetSizes = [100, 1000, 10000, 100000]
      radius = 0.05
      numNeighbors = 10
      rangeWidth = 0.05
  in  defaultMain [
      bgroup "linear-nn" $ map (nnLinearBench numQueries) pointSetSizes,
      bgroup "linear-rad" $ map (inRadLinearBench numQueries radius) pointSetSizes,
      bgroup "linear-knn" $ map (knnLinearBench numQueries numNeighbors) pointSetSizes,
      bgroup "linear-range" $ map (rangeLinearBench numQueries rangeWidth) pointSetSizes,
      bgroup "kdt-build" $ map buildKdtBench pointSetSizes,
      bgroup "kdt-nn" $ map (nnKdtBench numQueries) pointSetSizes,
      bgroup "kdt-rad" $ map (inRadKdtBench numQueries radius) pointSetSizes,
      bgroup "kdt-knn" $ map (knnKdtBench numQueries numNeighbors) pointSetSizes,
      bgroup "kdt-range" $ map (rangeKdtBench numQueries rangeWidth) pointSetSizes,
      bgroup "dkdt-nn" $ map nniDkdtBench pointSetSizes
      ]
