module Main where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector         as V
import qualified Data.List           as L

import           Data.Function       (on)

import Hammer.Math.Algebra
import System.Random

import qualified KdTree as KD
import qualified VpTree as VP

import Data.Time.Clock

instance KD.Point Vec3 where
  dimension _ = 3
  coord c (Vec3 x y z)
    | c == 0    = x
    | c == 1    = y
    | otherwise = z
  dist r1 r2 = norm $ r1 &- r2

instance VP.Metric Vec3 where
  dist = distSymm

main :: IO ()
main = testVP

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
  let x = tan (pi/8)
  return $ zipWith (&*) (randoms g) (randomRs (-x, x) g)

-- | Test if the distance function create a valid Metric space by sampling 3 rotations
-- within the Fundamental Zone. Returns Nothing if the metric is valid otherwise returns
-- the 3 rotations.
-- The tests were successful for rotations and fundamental zones (if all values are
-- inside the FZ)
testMetric :: IO (Maybe (Vec3, Vec3, Vec3))
testMetric = do
  [a, b, c] <- fmap (take 3) getFRFZ
  return $ if VP.dist a c <= (VP.dist a b) + (VP.dist b c) then Nothing else Just (a,b,c)

-- | Test KD tree on Euclidean space
testKD :: IO ()
testKD = let
  kd = KD.fromVector $ V.fromList [KD.Point3d 8 2 1, KD.Point3d 1 2 1, KD.Point3d 9 2 1]
  n  = KD.nearestNeighbor kd (KD.Point3d 15 2 1)
  in do
    print n
    print kd

-- | Test VP tree on Rodrigues-Frank space
testVP :: IO ()
testVP = do
  rs  <- fmap (U.fromList . take 10000) getFRFZ
  --[t] <- fmap (take 1) getFRFZ
  let
    --rs = [Vec3 x 0 0 | x <- let x = tan (pi/8) in [-x, 0.01-x .. x]]
    t = Vec3 (tan (pi/8) - 0.015) 0 0
    d = (5.1*pi/180)

    vp = VP.fromVector rs
    Just nvp@(ivp,_,_) = VP.nearestNeighbor vp t
    vpo = VP.nearNeighbors vp d t
    vplist = L.sort $ map (\(i,_,_) -> i) vpo

    bfos = U.filter ((< d) . distSymm t . snd) (U.imap (,) rs)
    nbfs@(ibfs,_) = U.minimumBy (compare `on` (distSymm t . snd)) bfos
    bflists = L.sort $ map fst $ U.toList bfos

    bfo = U.filter ((< d) . distNoSymm t . snd) (U.imap (,) rs)
    nbf = U.minimumBy (compare `on` (distNoSymm t . snd)) bfo
    bflist = L.sort $ map fst $ U.toList bfo

  putStrLn "==========Brutal Force (No Symm)=========="
  if (not $ U.null bfo) then (putStrLn $ "nearsest: " ++ show nbf) else return ()
  putStrLn $ "ix: " ++ show bflist
  putStrLn $ "#: " ++ show (length bflist)

  putStrLn "=========VP tree==========="
  putStrLn $ "nearest: " ++ show nvp
  putStrLn $ "ix: " ++ show vplist
  putStrLn $ "#: " ++ show (length vplist)

  putStrLn "==========Brutal Force=========="
  if (not $ U.null bfos) then (putStrLn $ "nearsest: " ++ show nbfs) else return ()
  putStrLn $ "ix: " ++ show bflists
  putStrLn $ "#: " ++ show (length bflists)

  putStrLn "========== Checking =========="
  if (not $ U.null bfos) then putStrLn ("check nearest: " ++ show (ivp == ibfs)) else return ()
  putStrLn $ "check nears: " ++ show (bflists == vplist)

  putStrLn "========== Performance =========="
  let nsample =  10000
  ps <- fmap (U.fromList . take nsample) getFRFZ

  ta0 <- getCurrentTime
  putStr $ "Sampling with brutal force " ++ show nsample ++ " rotations. Total points: "
  let func p = U.filter ((< d) . distNoSymm p . snd) (U.imap (,) rs)
  putStrLn $ show $ U.sum $ U.map (U.length . func) ps
  ta1 <- getCurrentTime
  putStrLn $ "Calculation time: " ++ show (diffUTCTime ta1 ta0)

  tb0 <- getCurrentTime
  putStr $ "Sampling with VP tree " ++ show nsample ++ " rotations. Total points: "
  putStrLn $ show $ U.sum $ U.map (length . VP.nearNeighbors vp d) ps
  tb1 <- getCurrentTime
  putStrLn $ "Calculation time: " ++ show (diffUTCTime tb1 tb0)

  tc0 <- getCurrentTime
  putStr $ "Searching nearest point in VP tree"
  [p1] <- fmap (take 1) getFRFZ
  putStrLn $ show $ VP.nearestNeighbor vp (p1)
  tc1 <- getCurrentTime
  putStrLn $ "Calculation time: " ++ show (diffUTCTime tc1 tc0)
