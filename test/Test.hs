{-# LANGUAGE FlexibleInstances #-}
module Main where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector         as V
import qualified Data.List           as L

import Data.Function (on)

import Hammer.Math.Algebra
import System.Random

import qualified Data.VPtree       as VP
import qualified Data.BlazeVPtree  as BVP

import Data.Time.Clock

instance VP.Metric (Double, Double, Double) where
  dist (a1,a2,a3) (b1,b2,b3) = let
    x = b1-a1
    y = b2-a2
    z = b3-a3
    in sqrt $ x*x + y*y + z*z

instance BVP.Metric (Double, Double, Double) where
  dist (a1,a2,a3) (b1,b2,b3) = let
    x = b1-a1
    y = b2-a2
    z = b3-a3
    in sqrt $ x*x + y*y + z*z

instance VP.Metric Vec3 where
  dist r1 r2 = len $ r1 &- r2

instance BVP.Metric Vec3 where
  dist r1 r2 = len $ r1 &- r2

instance VP.Metric Double where
  dist r1 r2 = abs $ r1 - r2

instance BVP.Metric Double where
  dist r1 r2 = abs $ r1 - r2

getD :: IO [Double]
getD = fmap (randomRs (0, 1)) newStdGen

getDDD :: IO [(Double, Double, Double)]
getDDD = do
  xs <- getD
  ys <- getD
  zs <- getD
  return $ zip3 xs ys zs

toVec3 = \(x,y,z) -> Vec3 x y z

main :: IO ()
main = do
  rs <- fmap (U.fromList . take 40000) getDDD
  t  <- fmap (U.head . U.fromList . take 1) getDDD
  --rs <- fmap (U.map toVec3 . U.fromList . take 40000) getDDD
  --t  <- fmap (toVec3 . U.head . U.fromList . take 1) getDDD
  let
    mvp = BVP.fromVector rs
    vp  = VP.fromVector rs
    d   = (0.01)

    vpo = VP.nearNeighbors vp d t
    vplist = L.sort $ map (\(i,_,_) -> i) vpo

    mvpo = BVP.nearNeighbors mvp d t
    mvplist = L.sort $ map (\(i,_,_) -> i) mvpo

    bfos = U.filter ((< d) . VP.dist t . snd) (U.imap (,) rs)
    bflists = L.sort $ map fst $ U.toList bfos

  putStrLn "=========VP tree==========="
  putStrLn $ "ix: " ++ show vplist
  putStrLn $ "#: " ++ show (length vplist)

  putStrLn "=========BVP tree==========="
  putStrLn $ "ix: " ++ show mvplist
  putStrLn $ "#: " ++ show (length mvplist)

  putStrLn "==========Brutal Force=========="
  putStrLn $ "ix: " ++ show bflists
  putStrLn $ "#: " ++ show (length bflists)

  putStrLn "========== Checking =========="
  putStrLn $ "check nears: " ++ show (bflists == vplist)

  putStrLn "========== Performance =========="
  let nsample =  10000
  --ps <- fmap (U.map toVec3 . U.fromList . take nsample) getDDD
  ps <- fmap (U.fromList . take nsample) getDDD

  printTime "brutal force" $ let
    func p = U.filter ((< d) . VP.dist p . snd) (U.imap (,) rs)
    in U.map (U.length . func) ps

  printTime "BlazeVP" $ U.map (length . BVP.nearNeighbors mvp d) ps

  printTime "VP tree" $ U.map (length . VP.nearNeighbors vp d) ps

printTime name ps = do
  ta0 <- getCurrentTime
  putStr $ "Sampling with " ++ name ++ ". Total points: "
  putStrLn $ show $ U.sum ps
  ta1 <- getCurrentTime
  putStrLn $ "Calculation time: " ++ show (diffUTCTime ta1 ta0)
