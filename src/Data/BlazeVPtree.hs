{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.BlazeVPtree
       ( Metric (..)
       , VPtree (..)
       , fromVector
       --, subtrees
       , nearNeighbors
       --, nearestNeighbor
       --, nearestThanNeighbor
       ) where

import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Generic.Mutable as M

import           Data.Function  (on)

import           Control.Monad.ST
import           Control.Monad
import           Data.Vector.Algorithms.Intro


import Debug.Trace

class Metric p where
  -- | Returns the distance between two points in the metric space.
  dist :: p -> p -> Double

data VPtree point
  = VPtree
  { vptree :: U.Vector (Int, point, Double)
  } deriving (Eq, Ord, Show)

fromVector :: (U.Unbox a, Metric a)=> U.Vector a -> VPtree a
fromVector v = VPtree $ runST $ do
  mv <- U.thaw (U.imap (\i p -> (i,p, fromIntegral i)) v)
  go mv (0, n-1)
  U.freeze mv
  where
    n = U.length v
    go mv range = do
      x <- mkBranch mv range
      maybe (return ()) (\(rangeIn, rangeOut) -> go mv rangeIn >> go mv rangeOut) x


mkBranch :: (Metric p, U.Unbox p)=> U.MVector s (Int, p, Double)
         -> (Int, Int) -> ST s (Maybe ((Int, Int), (Int, Int)))
mkBranch mv (il, iu)
  | il >  iu = return Nothing
  | il == iu = do
      (ip, p, _) <- M.read mv iu
      M.write mv iu (ip, p, 0)
      return Nothing
  | iu - il == 1 = do
      -- place farthest point at the lower index
      (_,_,dl) <- M.read mv il
      (_,_,du) <- M.read mv iu
      if dl < du then M.swap mv il iu else return ()
      -- calculate VP on the lower index because div (i+(i+1)) 2 = i
      -- therefore the sub-tree range (i, i+1) will always have i as VP
      (ip,  p,  _) <- M.read mv iu
      (ivp, vp, _) <- M.read mv il
      M.write mv iu (ip,  p,  0        )
      M.write mv il (ivp, vp, dist vp p)
      return Nothing
  | otherwise = do
      -- find farthest index
      (_,_,d0) <- M.read mv il
      let foo p@(acc, _) t = fmap (\(_,_,d) -> if d > acc then (d, t) else p) (M.read mv t)
      (_,imax) <- foldM foo (d0, il) [il + 1 .. iu]
      -- use farthest index as VP point and put at the begin
      M.swap mv imax il
      -- calculate distances from VP
      (ix, vp, _) <- M.read mv il
      let func i = do
            (ip, p, _) <- M.read mv i
            M.write mv i (ip, p, dist p vp)
      mapM_ func [il+1 .. iu]
      -- get middle length (median in sorted vector)
      let mid = (il + iu) `quot` 2
      -- sort according to the distance to VP point
      -- only sort half way until mid (the rest is not necessary)
      partialSortByBounds (compare `on` (\(_,_,d) -> d)) mv (mid-il) (il+1) (iu+1)
      -- cut at the median distance
      (_, _, median) <- M.read mv mid
      -- on space on the middle and place VP point
      M.swap mv il mid
      M.write mv mid (ix, vp, median)
      -- return branch's ranges
      return $ Just ((il, mid-1), (mid+1, iu))

-- | Finds all near neighbors within r distance of the tree.
nearNeighbors :: (U.Unbox p, Metric p) => VPtree p -> Double -> p -> [(Int, p, Double)]
nearNeighbors (VPtree v) radius q = go (0, U.length v - 1)
  where
    go (!il, !iu)
      | il >  iu   = []
      | isAllIn && not onlyIn = validPoint (goOut ++ allIn)
      | isAllIn    = validPoint allIn
      | onlyIn     = validPoint goIn
      | onlyOut    = validPoint goOut
      | both       = validPoint (goIn ++ goOut)
      | otherwise  = validPoint []
      where
        (ix, vp, mu) = v `U.unsafeIndex` mid
        mid     = (il + iu) `quot` 2
        goIn    = go (il, mid - 1)
        goOut   = go (mid + 1, iu)
        d       = dist q vp
        onlyOut = mu + radius < d
        onlyIn  = mu - radius > d
        both    = mu - radius <= d && d <= mu + radius
        isAllIn = d < radius - mu
        allIn   = map ((\(j, t, _) -> (j, t, dist q t)). U.unsafeIndex v) [il .. mid - 1]
        validPoint xs = if d <= radius then (ix, vp, d) : xs else xs



{--
-- | Finds the nearest neighbor point in the tree.
nearestThanNeighbor :: Metric p => VPtree p -> Double -> p -> Maybe (Int, p, Double)
nearestThanNeighbor = getWithRadius

-- | Finds the nearest neighbor point in the tree.
nearestNeighbor :: Metric p => VPtree p -> p -> Maybe (Int, p, Double)
nearestNeighbor VpEmpty _ = Nothing
nearestNeighbor n q = getWithRadius n (dist q (vpPoint n)) q

getWithRadius :: (Metric p)=> VPtree p -> Double -> p -> Maybe (Int, p, Double)
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
--}
