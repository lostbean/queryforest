{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module VpTree
       ( Metric (..)
       , VpTree
       , fromVector
       , subtrees
       , nearNeighbors
       , nearestNeighbor
       )where

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

import           System.Random

import Debug.Trace

class Metric p where
  -- | Returns the distance between two points in the metric space.
  dist :: p -> p -> Double

data VpTree point
  = VpNode
  { vpInBranch  :: VpTree point
  , vpOutBranch :: VpTree point
  , vpPoint     :: point
  , vpRef       :: Int
  , vpMu        :: Double
  }
  | VpEmpty
  deriving (Eq, Ord, Show)

fromVector :: ( G.Vector v p, G.Vector v (Int, p), G.Vector v (Int, p, Double)
              , G.Vector v Double, Metric p)=> v p -> VpTree p
fromVector v = fromVector' (mkStdGen $ G.length v) (G.imap (,) v)

fromVector' :: ( G.Vector v p, G.Vector v (Int, p), G.Vector v (Int, p, Double)
               , G.Vector v Double, Metric p)=> StdGen -> v (Int, p) -> VpTree p
fromVector' gen points
  | G.null points = VpEmpty
  | otherwise     = node
  where
    npoints = G.length points
    ivp = fst $ randomR (0, npoints - 1) gen

    ((ix, vp), ps) = popAt ivp points
    ds  = G.map (\(i, q) -> (i, q, dist vp q)) ps

    median
      | nds > 0   = (G.foldl' (\acc (_,_,d) -> acc + d) 0 ds) / (fromIntegral nds)
      | otherwise = 0
      where nds = G.length ds
    (outb, inb) = G.unstablePartition (\(_,_,d) -> (d > median)) ds
    -- Create node and construct subtrees
    node = VpNode { vpInBranch  = fromVector' gen (G.map (\(i,q,_) -> (i,q)) inb)
                  , vpOutBranch = fromVector' gen (G.map (\(i,q,_) -> (i,q)) outb)
                  , vpPoint     = vp
                  , vpRef       = ix
                  , vpMu        = median
                  }

popAt :: (M.MVector (G.Mutable v) a, G.Vector v a)=> Int -> v a -> (a, v a)
popAt i = (\v -> (G.head v, G.tail v)) . G.modify (\m -> M.swap m 0 i)

-- | Subtrees t returns a list containing t and all its subtrees, including the
-- empty leaf nodes.
subtrees :: VpTree p -> [VpTree p]
subtrees VpEmpty = [VpEmpty]
subtrees t@(VpNode ib ob _ _ _) = subtrees ib ++ [t] ++ subtrees ob

-- | Finds all near neighbors within r distance of the tree.
nearNeighbors :: Metric p => VpTree p -> Double -> p -> [(Int, p, Double)]
nearNeighbors VpEmpty _ _ = []
nearNeighbors (VpNode VpEmpty VpEmpty vp ix _) radius q
  | d <= radius = [(ix, vp, d)]
  | otherwise   = []
  where d = dist vp q
nearNeighbors (VpNode ib ob vp ix mu) radius q
  | goIn && goOut = maybePivot (nearNeighbors ib radius q ++ nearNeighbors ob radius q)
  | goIn          = maybePivot (nearNeighbors ib radius q)
  | otherwise     = maybePivot (nearNeighbors ob radius q)
  where
    d     = dist q vp
    goOut = d >= mu - radius
    goIn  = d <  mu + radius
    maybePivot xs = if d <= radius then (ix, vp, d) : xs else xs

-- | Finds the nearest neighbor point in the tree.
nearestNeighbor :: Metric p => VpTree p -> p -> Maybe (Int, p, Double)
nearestNeighbor = getWithRadius (1/0) -- Trick Infinity

getWithRadius :: (Metric p)=> Double -> VpTree p -> p -> Maybe (Int, p, Double)
getWithRadius _ VpEmpty _ = Nothing
getWithRadius _  (VpNode VpEmpty VpEmpty vp ix _) q = Just (ix, vp, dist vp q)
getWithRadius r0 (VpNode ib ob vp ix mu) q
  | goIn && goOut = getSmallest (getWithRadius r1 ib q) (getWithRadius r1 ob q)
  | goIn          = getSmallest (getWithRadius r1 ib q) Nothing
  | otherwise     = getSmallest Nothing                 (getWithRadius r1 ob q)
  where
    d     = dist q vp
    goOut = d >= mu - r1
    goIn  = d <  mu + r1
    r1 = if d <= r0 then d else r0
    compNode a@(_, _, ra) b@(_, _, rb) = if ra <= rb then a else b
    getSmallest (Just x) (Just y) = Just $ compNode x $ compNode y (ix, vp, d)
    getSmallest (Just x) _        = Just $ compNode x (ix, vp, d)
    getSmallest _        (Just y) = Just $ compNode y (ix, vp, d)
    getSmallest _        _        = Nothing
