{-# LANGUAGE FlexibleInstances #-}
module Main where

import System.Random
import qualified Data.Vector.Unboxed as U

import qualified Data.VPtree      as VP
import qualified Data.BlazeVPtree as BVP

import Data.Time.Clock

type V3 = (Double, Double, Double)

instance VP.Metric V3 where
  dist (a1,a2,a3) (b1,b2,b3) = let
    x = b1 - a1
    y = b2 - a2
    z = b3 - a3
    in sqrt $ x * x + y * y + z * z

instance BVP.Metric V3 where
  dist (a1,a2,a3) (b1,b2,b3) = let
    x = b1 - a1
    y = b2 - a2
    z = b3 - a3
    in sqrt $ x * x + y * y + z * z

getD :: IO [Double]
getD = fmap (randomRs (0, 1)) newStdGen

getDDD :: IO [V3]
getDDD = do
  xs <- getD
  ys <- getD
  zs <- getD
  return $ zip3 xs ys zs

main :: IO ()
main = do
  rs <- fmap (U.fromList . take 40000) getDDD
  let
    mvp = BVP.fromVector rs
    vp  = VP.fromVector rs
    d   = (1.5*pi / 180)

  putStrLn "========== Benchmark =========="
  let nsample =  10000
  ps <- fmap (U.fromList . take nsample) getDDD

  printTime "brutal force" $ let
    func p = U.filter ((< d) . VP.dist p . snd) (U.imap (,) rs)
    in U.map (U.length . func) ps

  printTime "BlazeVP" $ U.map (length . BVP.nearNeighbors mvp d) ps

  printTime "VP tree" $ U.map (length . VP.nearNeighbors vp d) ps

  printTime "BlazeVP nst" $ U.map ((\(Just (i,_,_)) -> i) . BVP.nearestNeighbor mvp) ps
  printTime "BlazeVP nst with 0 start" $ U.map ((\(Just (i,_,_)) -> i) . BVP.nearestThanNeighbor mvp 0) ps


printTime :: (U.Unbox s, Show s, Num s)=> [Char] -> U.Vector s -> IO ()
printTime name ps = do
  ta0 <- getCurrentTime
  putStr $ "Sampling with " ++ name ++ ". Total points: "
  putStrLn $ show $ U.sum ps
  ta1 <- getCurrentTime
  putStrLn $ "Calculation time: " ++ show (diffUTCTime ta1 ta0)
