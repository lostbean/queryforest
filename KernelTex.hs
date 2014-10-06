module Main where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector         as V
import qualified Data.List           as L

import qualified KdTree as KD
import qualified VpTree as VP

import Data.Function       (on)

import Hammer.Math.Algebra
import System.Random

import KdTree

import Debug.Trace

instance KD.Point Vec3 where
  dimension _ = 3
  coord c (Vec3 x y z)
    | c == 0    = x
    | c == 1    = y
    | otherwise = z
  dist2 r1 r2 = norm $ r1 &- r2

instance VP.Metric Vec3 where
  --dist r1 r2 = norm $ r1 &- r2
  dist = distRR

distRR r1 r2 =  2 * atan (norm r3)
  where
    r2' = neg r2
    r3  = (r1 &+ r2' &- (r1 &^ r2')) &* (1/(1 - r1 &. r2'))

getRR :: IO [Vec3]
getRR = newStdGen >>= \g -> return $ take 1000 $ zipWith (&*) (randoms g) (randomRs (0,10) g)

main = testRF

testMetric = do
  rs <- newStdGen >>= \g -> return $ take 3 $ zipWith (&*) (randoms g) (randomRs (0,100) g)
  let [a,b,c] = (rs :: [Vec3])
  return $ dist2 a c <= (dist2 a b) + (dist2 b c)

testCart = let
  kd = fromVector $ V.fromList [Point3d 8 2 1, Point3d 1 2 1, Point3d 9 2 1]
  n  = nearestNeighbor kd (Point3d 15 2 1)
  in do
    print n
    print kd

testRF = do
  rs <- getRR
  let
    --rs = [Vec3 x 0 0 | x <- [0,1..10]]
    t  = Vec3 100 20 (-1)
    d  = 1
    kd = KD.fromVector $ U.fromList rs
    vp = VP.fromVector $ U.fromList rs
    Just n = VP.nearestNeighbor vp t
    --ns  = nearNeighbors kd d t
    ns  = VP.nearNeighbors vp d t
  --putStrLn "===================="
  putStrLn $ "nearest: " ++ show n
  putStrLn "===================="
  putStrLn $ "nears: " ++ show ns
  putStrLn $ "ix: " ++ show (map (\(i,_,_) -> i) ns)
  putStrLn $ "#: " ++ show (length ns)
  putStrLn "===================="
  let ns2 = U.filter (\(_,_,dist) -> (dist <= d)) $ U.imap (\i p -> (i, p, VP.dist t p)) $ U.fromList rs
      n2 = U.minimumBy (compare `on` (\(_,_,dist) -> dist)) ns2
  if (not $ U.null ns2) then (putStrLn $ "nearsest2: " ++ show n2) else return ()
  putStrLn $ "nears2: " ++ show ns2
  putStrLn $ "ix2: " ++ show (U.map (\(i,_,_) -> i) ns2)
  putStrLn $ "#2: " ++ show (U.length ns2)
  putStrLn "===================="
  if (not $ U.null ns2) then
    putStrLn $ "check nearest: " ++ show (n == n2)
    else return ()
  putStrLn $ "check nears: " ++ show ((L.sort ns) == (L.sort $ U.toList ns2))
