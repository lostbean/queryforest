{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module KdTree where

import qualified Data.Foldable       as F
import qualified Data.List           as L
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G

import Data.Vector.Unboxed (Vector)
import Data.Function       (on)

import Data.Vector.Algorithms.Intro
import Control.Monad.ST
import Data.Maybe
import Test.QuickCheck

import Debug.Trace

class Point p where
      -- |dimension returns the number of coordinates of a point.
      dimension :: p -> Int

      -- |coord gets the k'th coordinate, starting from 0.
      coord :: Int -> p -> Double

      -- |dist2 returns the squared distance between two points.
      dist2 :: p -> p -> Double
      dist2 a b = sum . map diff2 $ [0..dimension a - 1]
        where diff2 i = abs (coord i a - coord i b)

-- |compareDistance p a b  compares the distances of a and b to p.
compareDistance :: (Point p) => p -> p -> p -> Ordering
compareDistance p a b = dist2 p a `compare` dist2 p b

data Point3d
  = Point3d
  { p3x :: Double
  , p3y :: Double
  , p3z :: Double
  } deriving (Eq, Ord, Show)

instance Point Point3d where
  dimension _ = 3

  coord 0 p = p3x p
  coord 1 p = p3y p
  coord _ p = p3z p


data KdTree point
  = KdNode
  { kdLeft  :: KdTree point
  , kdPoint :: point
  , kdRef   :: Int
  , kdRight :: KdTree point
  , kdAxis  :: Int
  }
  | KdEmpty
  deriving (Eq, Ord, Show)

instance Functor KdTree where
  fmap _ KdEmpty = KdEmpty
  fmap f (KdNode l x i r axis) = KdNode (fmap f l) (f x) i (fmap f r) axis

instance F.Foldable KdTree where
  foldr _ init1 KdEmpty = init1
  foldr f init1 (KdNode l x _ r _) = F.foldr f init3 l
    where
      init3 = f x init2
      init2 = F.foldr f init1 r

fromVector :: (G.Vector v p, G.Vector v (Int, p), Point p)=> v p -> KdTree p
fromVector points = fromVectorWithDepth (G.imap (,) points) 0

-- |fromListWithDepth selects an axis based on depth so that the axis cycles
-- through all valid values.
fromVectorWithDepth :: (G.Vector v (Int, p), Point p)=> v (Int, p) -> Int -> KdTree p
fromVectorWithDepth points depth
  | G.null points = KdEmpty
  | otherwise = node
  where
    axis = depth `mod` dimension (snd $ G.head points)

    -- Sort point list and choose median as pivot element
    sortedPoints = sortByAxis axis points
    medianIndex  = G.length sortedPoints `div` 2

    left   = G.take medianIndex       sortedPoints
    right  = G.drop (medianIndex + 1) sortedPoints
    (i, p) = sortedPoints G.! medianIndex
    -- Create node and construct subtrees
    node = KdNode { kdLeft  = fromVectorWithDepth left (depth + 1)
                  , kdPoint = p
                  , kdRef   = i
                  , kdRight = fromVectorWithDepth right (depth + 1)
                  , kdAxis  = axis }

sortByAxis :: (G.Vector v (Int, p), Point p)=> Int -> v (Int, p) -> v (Int, p)
sortByAxis axis v = runST $ do
  m <- G.unsafeThaw v
  sortBy (compare `on` (coord axis . snd)) m
  G.unsafeFreeze m

-- |subtrees t returns a list containing t and all its subtrees, including the
-- empty leaf nodes.
subtrees :: KdTree p -> [KdTree p]
subtrees KdEmpty = [KdEmpty]
subtrees t@(KdNode l _ _ r _) = subtrees l ++ [t] ++ subtrees r

-- |nearestNeighbor tree p returns the nearest neighbor of p in tree.
nearestNeighbor :: Point p => KdTree p -> p -> Maybe (Int, p)
nearestNeighbor KdEmpty _ = Nothing
nearestNeighbor (KdNode KdEmpty p i KdEmpty _) _ = Just (i, p)
nearestNeighbor (KdNode l p i r axis) probe
  | xProbe <= xp = findNearest l r
  | otherwise    = findNearest r l
  where
    xProbe = coord axis probe
    xp     = coord axis p
    d      = dist2 p probe
    findNearest tree1 tree2 = let
      candidates1 = case nearestNeighbor tree1 probe of
        Nothing    -> [(i, p)]
        Just best1 -> [best1, (i, p)]
      sphereIntersectsPlane = (abs $ xProbe - xp) <= d
      candidates2
        | sphereIntersectsPlane = candidates1 ++ maybeToList (nearestNeighbor tree2 probe)
        | otherwise = candidates1
      in Just . L.minimumBy (compareDistance probe `on` snd) $ candidates2

-- |nearNeighbors tree p returns all neighbors within distance r from p in tree.
nearNeighbors :: Point p => KdTree p -> Double -> p -> [(Int, p, Double)]
nearNeighbors KdEmpty _ _ = []
nearNeighbors (KdNode KdEmpty p i KdEmpty _) radius probe
  | d <= radius = [(i, p, d)]
  | otherwise   = []
  where d = dist2 p probe
nearNeighbors (KdNode l p i r axis) radius probe
  | xProbe <= xp = let
      nearest = maybePivot ++ nearNeighbors l radius probe
      in if xProbe + abs radius > xp
         then nearNeighbors r radius probe ++ nearest
         else nearest
  | otherwise = let
      nearest = maybePivot ++ nearNeighbors r radius probe
      in if xProbe - abs radius < xp
         then nearNeighbors l radius probe ++ nearest
         else nearest
  where
    xProbe     = coord axis probe
    xp         = coord axis p
    d          = dist2 probe p
    maybePivot = if d <= radius then [(i, p, d)] else []

instance Arbitrary Point3d where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Point3d x y z)
