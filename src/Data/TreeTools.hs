{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.TreeTools where

import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

import           Data.Function  (on)

import           Control.Monad.ST
import           Data.Vector.Algorithms.Intro
import           System.Random

popRandom :: (M.MVector (G.Mutable v) a, G.Vector v a)=> StdGen -> v a -> (a, v a)
popRandom gen xs = popAt (fst $ randomR (0, G.length xs - 1) gen) xs

popAt :: (M.MVector (G.Mutable v) a, G.Vector v a)=> Int -> v a -> (a, v a)
popAt i = (\v -> (G.head v, G.tail v)) . G.modify (\m -> M.swap m 0 i)

cutMedian :: (G.Vector v (Int, p, Double))=>
             v (Int, p, Double) -> (v (Int, p, Double), v (Int, p, Double), Double)
cutMedian v = (lower, upper, m)
  where
    i = G.length v `quot` 2
    (lower, upper) = G.splitAt i (sortSet v)
    getD (_ ,_ ,d) = d
    m | G.null lower && G.null upper = 0
      | G.null lower                 = getD (G.head upper)
      | G.null upper                 = getD (G.last lower)
      | otherwise                    = 0.5 * (getD (G.head upper) + getD (G.last lower))

sortSet :: (G.Vector v (Int, p, Double))=> v (Int, p, Double) -> v (Int, p, Double)
sortSet v = runST $ do
  m <- G.thaw v
  sortBy (compare `on` (\(_,_,d) -> d)) m
  G.unsafeFreeze m

selectVP :: (G.Vector v p)=> (p -> p -> Double) -> StdGen -> Int -> v p -> Maybe Int
selectVP func gen n xs
  | G.null xs = Nothing
  | otherwise = Just (fst out)
  where
    npoints = G.length xs
    is = U.fromList $ take (min n npoints) $ randomRs (0, npoints - 1) gen
    getSpread j js = let
      ds = G.map (func (xs G.! j) . (xs G.!)) js
      mu = G.sum ds / le
      le = (fromIntegral $ U.length ds)
      in G.sum (G.map (\x -> let y = x - mu in y*y) ds) / le
    out = G.foldl' (\x@(_, s0) i1 -> let
                       s1 = getSpread i1 is
                       in if s1 > s0 then (i1, s1) else x)
          (G.head is, getSpread (G.head is) is) (G.tail is)
