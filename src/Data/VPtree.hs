{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.VPtree
       ( Metric (..)
       , VpTree
       , fromVector
       , subtrees
       , toList
       , nearNeighbors
       , nearestNeighbor
       , nearestThanNeighbor
       ) where

import qualified Data.Vector.Generic as G

import           System.Random

import           Data.TreeTools

import Debug.Trace

class Metric p where
  -- | Returns the distance between two points in the metric space.
  dist :: p -> p -> Double

data VpTree point
  = VpNode
  { vpInBranch  :: VpTree point
  , vpOutBranch :: VpTree point
  , vpPoint     :: point
  , vpRef       :: {-# UNPACK #-} !Int
  , vpMu        :: {-# UNPACK #-} !Double
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
    ((ix, vp), ps) = popAt (G.length points - 1) points
    -- ((ix, vp), ps) = popRandom gen points
    ds = G.map (\(i, q) -> (i, q, dist vp q)) ps

    (inb, outb, median) = cutMedian ds
    -- Create node and construct subtrees
    node = VpNode { vpInBranch  = fromVector' gen (G.map (\(i,q,_) -> (i,q)) inb)
                  , vpOutBranch = fromVector' gen (G.map (\(i,q,_) -> (i,q)) outb)
                  , vpPoint     = vp
                  , vpRef       = ix
                  , vpMu        = median
                  }

-- | Subtrees t returns a list containing t and all its subtrees, including the
-- empty leaf nodes.
subtrees :: VpTree p -> [VpTree p]
subtrees VpEmpty = [VpEmpty]
subtrees t@(VpNode ib ob _ _ _) = subtrees ib ++ [t] ++ subtrees ob

toList :: VpTree p -> [(Int, p)]
toList VpEmpty = []
toList (VpNode ib ob vp ix _) = (ix, vp) : (toList ib ++ toList ob)

-- | Finds all near neighbors within r distance of the tree.
nearNeighbors :: Metric p => VpTree p -> Double -> p -> [(Int, p, Double)]
nearNeighbors VpEmpty _ _ = []
nearNeighbors (VpNode VpEmpty VpEmpty vp ix _) radius q
  | d <= radius = [(ix, vp, d)]
  | otherwise   = []
  where d = dist vp q
nearNeighbors (VpNode ib ob vp ix mu) radius q
  | isAllIn &&
    not onlyIn = validPoint (nearNeighbors ob radius q ++ allIn)
  | isAllIn = validPoint allIn
  | onlyIn  = validPoint (nearNeighbors ib radius q)
  | onlyOut = validPoint (nearNeighbors ob radius q)
  | both    = validPoint (nearNeighbors ib radius q ++ nearNeighbors ob radius q)
  | otherwise = validPoint []
  where
    d       = dist q vp
    onlyOut = mu + radius < d
    onlyIn  = mu - radius > d
    both    = mu - radius <= d && d <= mu + radius
    isAllIn = d < radius - mu
    allIn   = map (\(j, t) -> (j, t, dist q t)) (toList ib )
    validPoint xs = if d <= radius then (ix, vp, d) : xs else xs

-- | Finds the nearest neighbor point in the tree.
nearestThanNeighbor :: Metric p => VpTree p -> Double -> p -> Maybe (Int, p, Double)
nearestThanNeighbor = getWithRadius

-- | Finds the nearest neighbor point in the tree.
nearestNeighbor :: Metric p => VpTree p -> p -> Maybe (Int, p, Double)
nearestNeighbor VpEmpty _ = Nothing
nearestNeighbor n q = getWithRadius n (dist q (vpPoint n)) q

getWithRadius :: (Metric p)=> VpTree p -> Double -> p -> Maybe (Int, p, Double)
getWithRadius VpEmpty _ _ = Nothing
getWithRadius (VpNode VpEmpty VpEmpty vp ix _) _ q = Just (ix, vp, dist vp q)
getWithRadius (VpNode ib ob vp ix mu) r0 q
  | goBoth &&
    d < mu    = getSmallest2 (\r -> getWithRadius ib r q) (\r -> getWithRadius ob r q) std
  | goBoth    = getSmallest2 (\r -> getWithRadius ob r q) (\r -> getWithRadius ib r q) std
  | goIn      = getSmallest  (\r -> getWithRadius ib r q) std
  | otherwise = getSmallest  (\r -> getWithRadius ob r q) std
  where
    d      = dist q vp
    r1     = if d <= r0 then d else r0
    std    = (ix, vp, d)
    goOut  = d >= mu - r1
    goIn   = d <= mu + r1
    goBoth = goIn && goOut

    compNode !a@(_, _, ra) !b@(_, _, rb) = if ra <= rb then a else b
    getSmallest  f1    = Just . fuu f1
    getSmallest2 f1 f2 = Just . fuu f2 . fuu f1
    fuu func !x@(_, _, dx) = maybe x (compNode x) (func dx)
