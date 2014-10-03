module Main where

import KdTree
import Hammer.Math.Algebra
import System.Random

import Debug.Trace

instance Point Vec3 where
  dimension _ = 3
  coord c (Vec3 x y z)
    | c == 0 = x
    | c == 1 = y
    | otherwise = z
  dist2 r1 r2 = 2 * atan (norm r3)
    where
      r2' = neg r2
      r3  = (r1 &+ r2' &- (r1 &^ r2')) &* (1/(1 - r1 &. r2'))

getRR :: IO [Vec3]
getRR = newStdGen >>= \g -> return $ take 1000 $ zipWith (&*) (randoms g) (randomRs (0,2) g)

main = testRF

testCart = let
  kd = fromList [Point3d 1 2 1, Point3d 9 2 1]
  n = nearestNeighbor kd (Point3d 3 2 1)
  in print n

testRF = do
  rs <- getRR
  let
    t  = Vec3 1 1 1
    d  = 0.5
    kd = fromList rs
    n   = nearestNeighbor kd t
    ns  = nearNeighbors kd (sqrt d) t
  print kd
  putStrLn "===================="
  putStrLn $ "nearest: " ++ show n ++ " - dist: " ++ show (fmap (dist2 t) n)
  putStrLn "===================="
  putStrLn $ "nears: " ++ show ns
  putStrLn $ "dist: " ++ show (map (dist2 t) ns)
  putStrLn "===================="
  let ns2 = filter ((<= d) . trace "!" . dist2 t) rs
  putStrLn $ "nears2: " ++ show ns2
  putStrLn $ "dist2: " ++ show (map (dist2 t) ns2)
