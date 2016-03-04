module Data.KdTree.Dynamic
       ( -- * Usage

         -- $usage

         -- * Reference

         -- ** Types
         PointAsListFn
       , SquaredDistanceFn
       , KdTree
         -- ** Dynamic /k/-d tree construction
       , empty
       , singleton
       , emptyWithDist
       , singletonWithDist
         -- ** Insertion
       , insert
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

import Prelude hiding (null)

import qualified Data.Foldable as F

import qualified Data.KdMap.Dynamic as DKDM
import Data.KdMap.Dynamic (PointAsListFn, SquaredDistanceFn, defaultSqrDist)

-- $usage
--
-- The 'KdTree' is a dynamic variant of
-- @Data.KdTree.Static.@'Data.KdTree.Static.KdTree' that allows for
-- insertion of new points into an existing 'KdTree'. This algorithm
-- was implemented using a
-- <http://repository.cmu.edu/cgi/viewcontent.cgi?article=3453&context=compsci static-to-dynamic transformation>.
--
-- Here's an example of interleaving 3D point insertions and point
-- queries using 'KdTree':
--
-- @
-- >>> let dkdt = 'singleton' point3dAsList (Point3D 0.0 0.0 0.0)
--
-- >>> let dkdt' = 'insert' dkdt (Point3D 1.0 1.0 1.0)
--
-- >>> 'nearest' dkdt' (Point3D 0.4 0.4 0.4)
-- Point3D {x = 0.0, y = 0.0, z = 0.0}
--
-- >>> let dkdt'' = 'insert' dkdt' (Point3D 0.5 0.5 0.5)
--
-- >>> 'nearest' dkdt'' (Point3D 0.4 0.4 0.4)
-- Point3D {x = 0.5, y = 0.5, z = 0.5}
-- @
--
-- Check out @Data.KdMap.Dynamic.@'Data.KdMap.Dynamic.KdMap' if you
-- want to associate a value with each point in your tree structure.

-- | A dynamic /k/-d tree structure that stores points of type @p@
-- with axis values of type @a@.
newtype KdTree a p = KdTree (DKDM.KdMap a p ())

instance F.Foldable (KdTree a) where
  foldr f z (KdTree dkdMap) = DKDM.foldrWithKey (f . fst) z dkdMap

instance (Show a, Show p) => Show (KdTree a p) where
  show (KdTree kdm) = "KdTree " ++ show kdm

-- | Generates an empty 'KdTree' with a user-specified distance function.
emptyWithDist :: Real a => PointAsListFn a p -> SquaredDistanceFn a p -> KdTree a p
emptyWithDist p2l d2 = KdTree $ DKDM.emptyWithDist p2l d2

-- | Generates an empty 'KdTree' with the default distance function.
empty :: Real a => PointAsListFn a p -> KdTree a p
empty p2l = emptyWithDist p2l $ defaultSqrDist p2l

-- | Returns whether the 'KdTree' is empty.
null :: KdTree a p -> Bool
null (KdTree dkdMap) = DKDM.null dkdMap

-- | Generates a 'KdTree' with a single point using a
-- user-specified distance function.
singletonWithDist :: Real a => PointAsListFn a p
                            -> SquaredDistanceFn a p
                            -> p
                            -> KdTree a p
singletonWithDist p2l d2 p = KdTree $ DKDM.singletonWithDist p2l d2 (p, ())

-- | Generates a 'KdTree' with a single point using the default
-- distance function.
singleton :: Real a => PointAsListFn a p -> p -> KdTree a p
singleton p2l = singletonWithDist p2l $ defaultSqrDist p2l

-- | Adds a given point to a 'KdTree'.
--
-- Average time complexity per insert for /n/ inserts: /O(log^2(n))/.
insert :: Real a => KdTree a p -> p -> KdTree a p
insert (KdTree dkdMap) p = KdTree $ DKDM.insert dkdMap p ()

-- | Given a 'KdTree' and a query point, returns the nearest point
-- in the 'KdTree' to the query point.
--
-- Average time complexity: /O(log^2(n))/.
nearest :: Real a => KdTree a p -> p -> p
nearest (KdTree dkdMap) = fst . DKDM.nearest dkdMap

-- | Given a 'KdTree', a query point, and a number @k@, returns the
-- @k@ nearest points in the 'KdTree' to the query point.
--
-- Neighbors are returned in order of increasing distance from query
-- point.
--
-- Average time complexity: /log(k) * log^2(n)/ for /k/ nearest
-- neighbors on a structure with /n/ data points.
--
-- Worst case time complexity: /n * log(k)/ for /k/ nearest neighbors
-- on a structure with /n/ data points.
kNearest :: Real a => KdTree a p -> Int -> p -> [p]
kNearest (KdTree dkdMap) k query =
  map fst $ DKDM.kNearest dkdMap k query

-- | Given a 'KdTree', a query point, and a radius, returns all
-- points in the 'KdTree' that are within the given radius of the
-- query points.
--
-- Points are not returned in any particular order.
--
-- Worst case time complexity: /O(n)/ for /n/ data points.
inRadius :: Real a => KdTree a p -> a -> p -> [p]
inRadius (KdTree dkdMap) radius query =
  map fst $ DKDM.inRadius dkdMap radius query

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
inRange (KdTree dkdMap) lowers uppers =
  map fst $ DKDM.inRange dkdMap lowers uppers

-- | Returns the number of elements in the 'KdTree'.
--
-- Time complexity: /O(1)/
size :: KdTree a p -> Int
size (KdTree dkdMap) = DKDM.size dkdMap

-- | Returns a list of all the points in the 'KdTree'.
--
-- Time complexity: /O(n)/
toList :: KdTree a p -> [p]
toList (KdTree dkdMap) = DKDM.keys dkdMap
