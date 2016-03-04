{-# LANGUAGE DeriveGeneric #-}

module Data.KdTree.Static
       ( -- * Introduction

         -- $intro

         -- * Usage

         -- $usage

         -- * Variants

         -- ** Dynamic /k/-d trees

         -- $dkdtrees

         -- ** /k/-d maps

         -- $kdmaps

         -- * Advanced

         -- ** Custom distance functions

         -- $customdistancefunctions

         -- ** Axis value types

         -- $axisvaluetypes

         -- * Reference

         -- ** Types
         PointAsListFn
       , SquaredDistanceFn
       , KdTree
         -- ** /k/-d tree construction
       , empty
       , emptyWithDist
       , singleton
       , singletonWithDist
       , build
       , buildWithDist
       , insertUnbalanced
       , batchInsertUnbalanced
         -- ** Query
       , nearest
       , inRadius
       , kNearest
       , inRange
       , toList
       , null
       , size
         -- ** Utilities
       , defaultSqrDist
       ) where

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics

import qualified Data.Foldable as F
import Prelude hiding (null)

import qualified Data.KdMap.Static as KDM
import Data.KdMap.Static (PointAsListFn, SquaredDistanceFn, defaultSqrDist)

-- $intro
--
-- Let's say you have a large set of 3D points called /data points/,
-- and you'd like to be able to quickly perform /point queries/ on the
-- data points. One example of a point query is the /nearest neighbor/
-- query: given a set of data points @points@ and a query point @p@,
-- which point in @points@ is closest to @p@?
--
-- We can efficiently solve the nearest neighbor query (along with
-- many other types of point queries) if we appropriately organize the
-- data points. One such method of organization is called the /k/-d
-- tree algorithm, which is implemented in this module.

-- $usage
--
-- Let's say you have a list of 3D data points, and each point is of
-- type @Point3d@:
--
-- @
-- data Point3d = Point3d { x :: Double
--                        , y :: Double
--                        , z :: Double
--                        } deriving Show
-- @
--
-- We call a point's individual values /axis values/ (i.e., @x@, @y@,
-- and @z@ in the case of @Point3d@).
--
-- In order to generate a /k/-d tree of @Point3d@'s, we need to define
-- a 'PointAsListFn' that expresses the point's axis values as a list:
--
-- @
-- point3dAsList :: Point3d -> [Double]
-- point3dAsList (Point3d x y z) = [x, y, z]
-- @
--
-- Now we can build a 'KdTree' structure from a list of data points
-- and perform a nearest neighbor query as follows:
--
-- @
-- >>> let dataPoints = [(Point3d 0.0 0.0 0.0), (Point3d 1.0 1.0 1.0)]
--
-- >>> let kdt = 'build' point3dAsList dataPoints
--
-- >>> let queryPoint = Point3d 0.1 0.1 0.1
--
-- >>> 'nearest' kdt queryPoint
-- Point3d {x = 0.0, y = 0.0, z = 0.0}
-- @

-- $dkdtrees
--
-- The 'KdTree' structure is meant for static sets of data points. If
-- you need to insert points into an existing /k/-d tree, check out
-- @Data.KdTree.Dynamic.@'Data.KdTree.Dynamic.KdTree'.

-- $kdmaps
--
-- If you need to associate additional data with each point in the
-- tree (i.e., points are /keys/ associated with /values/), check out
-- @Data.KdMap.Static.@'Data.KdMap.Static.KdMap' and
-- @Data.KdMap.Dynamic.@'Data.KdMap.Dynamic.KdMap' for static and dynamic
-- variants of this functionality. Please /do not/ try to fake this
-- functionality with a 'KdTree' by augmenting your point type with
-- the extra data; you're gonna have a bad time.

-- $customdistancefunctions
--
-- You may have noticed in the previous use case that we never
-- specified what "nearest" means for our points. By default,
-- 'build' uses a Euclidean distance function that is sufficient
-- in most cases. However, point queries are typically faster on a
-- 'KdTree' built with a user-specified custom distance
-- function. Let's generate a 'KdTree' using a custom distance
-- function.
--
-- One idiosyncrasy about 'KdTree' is that custom distance functions
-- are actually specified as /squared distance/ functions
-- ('SquaredDistanceFn'). This means that your custom distance
-- function must return the /square/ of the actual distance between
-- two points. This is for efficiency: regular distance functions
-- often require expensive square root computations, whereas in our
-- case, the squared distance works fine and doesn't require computing
-- any square roots. Here's an example of a squared distance function
-- for @Point3d@:
--
-- @
-- point3dSquaredDistance :: Point3d -> Point3d -> Double
-- point3dSquaredDistance (Point3d x1 y1 z1) (Point3d x2 y2 z2) =
--   let dx = x1 - x2
--       dy = y1 - y2
--       dz = z1 - z2
--   in  dx * dx + dy * dy + dz * dz
-- @
--
-- We can build a 'KdTree' using our custom distance function as follows:
--
-- @
-- >>> let kdt = 'buildWithDist' point3dAsList point3dSquaredDistance points
-- @

-- $axisvaluetypes
--
-- In the above examples, we used a point type with axis values of
-- type 'Double'. We can in fact use axis values of any type that is
-- an instance of the 'Real' typeclass. This means you can use points
-- that are composed of 'Double's, 'Int's, 'Float's, and so on:
--
-- @
-- data Point2i = Point2i Int Int
--
-- point2iAsList :: Point2i -> [Int]
-- point2iAsList (Point2i x y) = [x, y]
--
-- kdt :: [Point2i] -> KdTree Int Point2i
-- kdt dataPoints = 'build' point2iAsList dataPoints
-- @

-- | A /k/-d tree structure that stores points of type @p@ with axis
-- values of type @a@.
newtype KdTree a p = KdTree (KDM.KdMap a p ()) deriving Generic
instance (NFData a, NFData p) => NFData (KdTree a p) where rnf = genericRnf

instance (Show a, Show p) => Show (KdTree a p) where
  show (KdTree kdm) = "KdTree " ++ show kdm

instance F.Foldable (KdTree a) where
  foldr f z (KdTree kdMap) = KDM.foldrWithKey (f . fst) z kdMap

-- | Builds an empty 'KdTree'.
empty :: Real a => PointAsListFn a p -> KdTree a p
empty = KdTree . KDM.empty

-- | Builds an empty 'KdTree' using a user-specified squared distance
-- function.
emptyWithDist :: Real a => PointAsListFn a p
                        -> SquaredDistanceFn a p
                        -> KdTree a p
emptyWithDist p2l d2 = KdTree $ KDM.emptyWithDist p2l d2

-- | Builds a 'KdTree' with a single point.
singleton :: Real a => PointAsListFn a p -> p -> KdTree a p
singleton p2l p = KdTree $ KDM.singleton p2l (p, ())

-- | Builds a 'KdTree' with a single point using a user-specified
-- squared distance function.
singletonWithDist :: Real a => PointAsListFn a p
                            -> SquaredDistanceFn a p
                            -> p
                            -> KdTree a p
singletonWithDist p2l d2 p = KdTree $ KDM.singletonWithDist p2l d2 (p, ())

null :: KdTree a p -> Bool
null (KdTree kdm) = KDM.null kdm

-- | Builds a 'KdTree' from a list of data points using a default
-- squared distance function 'defaultSqrDist'.
--
-- Average complexity: /O(n * log(n))/ for /n/ data points.
--
-- Worst case time complexity: /O(n^2)/ for /n/ data points.
--
-- Worst case space complexity: /O(n)/ for /n/ data points.
build :: Real a => PointAsListFn a p
                   -> [p] -- ^ non-empty list of data points to be stored in the /k/-d tree
                   -> KdTree a p
build pointAsList ps =
  KdTree $ KDM.build pointAsList $ zip ps $ repeat ()

-- | Builds a 'KdTree' from a list of data points using a
-- user-specified squared distance function.
--
-- Average time complexity: /O(n * log(n))/ for /n/ data points.
--
-- Worst case time complexity: /O(n^2)/ for /n/ data points.
--
-- Worst case space complexity: /O(n)/ for /n/ data points.
buildWithDist :: Real a => PointAsListFn a p
                        -> SquaredDistanceFn a p
                        -> [p]
                        -> KdTree a p
buildWithDist pointAsList distSqr ps =
  KdTree $ KDM.buildWithDist pointAsList distSqr $ zip ps $ repeat ()

-- | Inserts a point into a 'KdTree'. This can potentially
-- cause the internal tree structure to become unbalanced. If the tree
-- becomes too unbalanced, point queries will be very inefficient. If
-- you need to perform lots of point insertions on an already existing
-- /k/-d tree, check out
-- @Data.KdTree.Dynamic.@'Data.KdTree.Dynamic.KdTree'.
--
-- Average complexity: /O(log(n))/ for /n/ data points.
--
-- Worse case time complexity: /O(n)/ for /n/ data points.
insertUnbalanced :: Real a => KdTree a p -> p -> KdTree a p
insertUnbalanced (KdTree kdm) p = KdTree $ KDM.insertUnbalanced kdm p ()

-- | Inserts a list of points into a 'KdTree'. This can potentially
-- cause the internal tree structure to become unbalanced, which leads
-- to inefficient point queries.
--
-- Average complexity: /O(n * log(n))/ for /n/ data points.
--
-- Worst case time complexity: /O(n^2)/ for /n/ data points.
batchInsertUnbalanced :: Real a => KdTree a p -> [p] -> KdTree a p
batchInsertUnbalanced (KdTree kdm) ps =
  KdTree $ KDM.batchInsertUnbalanced kdm $ zip ps $ repeat ()

-- | Given a 'KdTree' and a query point, returns the nearest point
-- in the 'KdTree' to the query point.
--
-- Average time complexity: /O(log(n))/ for /n/ data points.
--
-- Worst case time complexity: /O(n)/ for /n/ data points.
--
-- Throws an error if called on an empty 'KdTree'.
nearest :: Real a => KdTree a p -> p -> p
nearest (KdTree t) query
  | KDM.null t = error "Attempted to call nearest on an empty KdTree."
  | otherwise = fst $ KDM.nearest t query

-- | Given a 'KdTree', a query point, and a radius, returns all
-- points in the 'KdTree' that are within the given radius of the
-- query point.
--
-- Points are not returned in any particular order.
--
-- Worst case time complexity: /O(n)/ for /n/ data points and
-- a radius that subsumes all points in the structure.
inRadius :: Real a => KdTree a p
                   -> a -- ^ radius
                   -> p -- ^ query point
                   -> [p] -- ^ list of points in tree with given
                          -- radius of query point
inRadius (KdTree t) radius query = map fst $ KDM.inRadius t radius query

-- | Given a 'KdTree', a query point, and a number @k@, returns the
-- @k@ nearest points in the 'KdTree' to the query point.
--
-- Neighbors are returned in order of increasing distance from query
-- point.
--
-- Average time complexity: /log(k) * log(n)/ for /k/ nearest
-- neighbors on a structure with /n/ data points.
--
-- Worst case time complexity: /n * log(k)/ for /k/ nearest
-- neighbors on a structure with /n/ data points.
kNearest :: Real a => KdTree a p -> Int -> p -> [p]
kNearest (KdTree t) k query = map fst $ KDM.kNearest t k query

-- | Finds all points in a 'KdTree' with points within a given range,
-- where the range is specified as a set of lower and upper bounds.
--
-- Points are not returned in any particular order.
--
-- Worst case time complexity: /O(n)/ for n data points and a range
-- that spans all the points.
inRange :: Real a => KdTree a p
                  -> p -- ^ lower bounds of range
                  -> p -- ^ upper bounds of range
                  -> [p] -- ^ all points within given range
inRange (KdTree t) lower upper = map fst $ KDM.inRange t lower upper

-- | Returns a list of all the points in the 'KdTree'.
--
-- Time complexity: /O(n)/ for /n/ data points.
toList :: KdTree a p -> [p]
toList (KdTree t) = KDM.keys t

-- | Returns the number of elements in the 'KdTree'.
--
-- Time complexity: /O(1)/
size :: KdTree a p -> Int
size (KdTree t) = KDM.size t
