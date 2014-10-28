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
  --dist r1 r2 = len $ r1 &- r2
  dist = distSymm

instance BVP.Metric Vec3 where
  --dist r1 r2 = len $ r1 &- r2
  dist = distSymm

instance VP.Metric Double where
  dist r1 r2 = abs $ r1 - r2

instance BVP.Metric Double where
  dist r1 r2 = abs $ r1 - r2


-- | Distance between two rotations without any symmetry
distNoSymm :: Vec3 -> Vec3 -> Double
distNoSymm r1 r2 = 2 * atan (norm $ rComp r1 (neg r2))

-- | Distance between two rotations regarding symmetry
distSymm :: Vec3 -> Vec3 -> Double
distSymm r1 r2 = minimum $ map (distNoSymm r1 . rComp r2) [zero, Vec3 1 0 0, Vec3 (-1) 0 0]

-- | Compose Rodigues-Frank
rComp :: Vec3 -> Vec3 -> Vec3
rComp r1 r2 = (r1 &+ r2 &- (r1 &^ r2)) &* (1/(1 - r1 &. r2))

-- | Generate random values of Rodrigues-Frank C4 symmetry (90 <100>, 90 <010> 90 <001>)
-- with FZ planes at (+-45 <100>,+-45 <010>,+-45 <001>)
getFRFZ :: IO [Vec3]
getFRFZ = newStdGen >>= \g -> do
  return $ zipWith (\v k -> v &* (k * tan (pi/8))) (randoms g) (randomRs (0, 1) g)

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
  --rs <- fmap (U.fromList . take 10000) getFRFZ
  --t  <- fmap (U.head . U.fromList . take 1) getFRFZ
  rs <- fmap (U.fromList . take 40000) getDDD
  t  <- fmap (U.head . U.fromList . take 1) getDDD
  --rs <- fmap (U.map toVec3 . U.fromList . take 40000) getDDD
  --t  <- fmap (toVec3 . U.head . U.fromList . take 1) getDDD
  let
    mvp = BVP.fromVector rs
    vp  = VP.fromVector rs
    --t = Vec3 (tan (pi/8) - 0.015) 0 0
    d = (1.5*pi/180)

    vpo    = VP.nearNeighbors vp d t
    vpnst  = VP.nearestNeighbor vp t
    vplist = L.sort $ map (\(i,_,_) -> i) vpo

    mvpo    = BVP.nearNeighbors mvp d t
    mvpnst  = BVP.nearestNeighbor mvp t
    mvplist = L.sort $ map (\(i,_,_) -> i) mvpo

    bfo    = U.filter ((< d) . VP.dist t . snd) (U.imap (,) rs)
    bfnst  = U.minimumBy (compare `on` (VP.dist t . snd)) bfo
    bflist = L.sort $ map fst $ U.toList bfo

  putStrLn "=========VP tree==========="
  putStrLn $ "nst: " ++ show vpnst
  putStrLn $ "ix: " ++ show vplist
  putStrLn $ "#: " ++ show (length vplist)

  putStrLn "=========BVP tree==========="
  putStrLn $ "nst: " ++ show mvpnst
  putStrLn $ "ix: " ++ show mvplist
  putStrLn $ "#: " ++ show (length mvplist)

  putStrLn "==========Brutal Force=========="
  if (not $ U.null bfo) then (putStrLn $ "nst: " ++ show bfnst) else return ()
  putStrLn $ "ix: " ++ show bflist
  putStrLn $ "#: " ++ show (length bflist)

  putStrLn "========== Checking =========="
  putStrLn $ "check nears: " ++ show (bflist == vplist)

  --print $ map (\(i,_,d) -> (i, d)) $ VP.flatTree vp
  --print $ U.map (\(i,_,d) -> (i, d)) $ BVP.vptree mvp

  putStrLn "========== Performance =========="
  let nsample =  10000
  --ps <- fmap (U.fromList . take nsample) getFRFZ
  ps <- fmap (U.fromList . take nsample) getDDD
  --ps <- fmap (U.map toVec3 . U.fromList . take nsample) getDDD

  printTime "brutal force" $ let
    func p = U.filter ((< d) . VP.dist p . snd) (U.imap (,) rs)
    in U.map (U.length . func) ps

  printTime "BlazeVP" $ U.map (length . BVP.nearNeighbors mvp d) ps

  printTime "VP tree" $ U.map (length . VP.nearNeighbors vp d) ps

  printTime "VP nst" $ U.map ((\(Just (i,_,_)) -> i) . BVP.nearestThanNeighbor mvp (d/2)) ps

printTime :: (U.Unbox s, Show s, Num s)=> [Char] -> U.Vector s -> IO ()
printTime name ps = do
  ta0 <- getCurrentTime
  putStr $ "Sampling with " ++ name ++ ". Total points: "
  putStrLn $ show $ U.sum ps
  ta1 <- getCurrentTime
  putStrLn $ "Calculation time: " ++ show (diffUTCTime ta1 ta0)
