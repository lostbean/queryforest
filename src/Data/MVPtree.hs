{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.MVPtree
       ( Metric (..)
       , VPtree
       , fromVector
       --, subtrees
       , nearNeighbors
       --, nearestNeighbor
       )where

import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Generic         as G

import           Data.Function  (on)

import           Control.Concurrent.MVar
import           System.Random

import           Data.TreeTools

import Debug.Trace

class Metric p where
  -- | Returns the distance between two points in the metric space.
  dist :: p -> p -> Double

type Path  = V.Vector (U.Vector Double, U.Vector Double)
type Level = Int

data VPtree point
  = VpNode
  { nodeSS1L :: VPtree point
  , nodeMa   :: Double
  , nodeSS1U :: VPtree point
  , nodeMb   :: Double
  , nodeSS2L :: VPtree point
  , nodeMc   :: Double
  , nodeSS2U :: VPtree point
  , nodeVP1  :: point
  , nodeVP2  :: point
  , nodeIx1  :: Int
  , nodeIx2  :: Int
  }
  | VpLeaf
  { leafVP1   :: point
  , leafVP2   :: point
  , leafIx1   :: Int
  , leafIx2   :: Int
  , leafPS    :: U.Vector (Int, point, Double, Double)
  }
  | VpSingleLeaf
  { leafVP :: point
  , leafIx :: Int
  }
  | VpEmpty
  deriving (Eq, Ord, Show)


data MVPCfg = MVPCfg {extraLeafPoints :: Int, histLength :: Int} deriving (Show)

fromVector :: ( G.Vector v (Int, Double), G.Vector v (Int, p), G.Vector v (Int, p, Double)
              , G.Vector v (Int, p, Double, Double), G.Vector v p, Metric p, U.Unbox p)
              => v p -> IO (VPtree p, Path)
fromVector points = do
  let
    ps  = G.imap (,) points
    ds0 = U.replicate (G.length ps) 0
  mpath <- newMVar (V.singleton (ds0, ds0))
  tree  <- mkTree (MVPCfg 4 10) (mkStdGen (G.length ps)) 0 mpath ps
  path  <- readMVar mpath
  return (tree, path)

mkTree :: ( G.Vector v (Int, Double), G.Vector v (Int, p), G.Vector v (Int, p, Double)
          , G.Vector v (Int, p, Double, Double), Metric p ,U.Unbox p )
          => MVPCfg -> StdGen -> Level -> MVar Path -> v (Int, p) -> IO (VPtree p)
mkTree cfg gen level mpath points
  | G.null points = return VpEmpty
  | isSingleLeaf  = let (ix, vp) = G.head points in return (VpSingleLeaf vp ix)
  | isLeaf        = return (genLeaf cfg gen points)
  | otherwise     = genNode cfg gen level mpath points
  where
    isLeaf = G.length points <= (extraLeafPoints cfg) + 2
    isSingleLeaf = G.length points == 1

genNode :: ( G.Vector v (Int, Double), G.Vector v (Int, p), G.Vector v (Int, p, Double)
           , G.Vector v (Int, p, Double, Double), Metric p, U.Unbox p )
           => MVPCfg -> StdGen -> Level -> MVar Path -> v (Int, p) -> IO (VPtree p)
genNode cfg gen level mpath points = do
  updatePath mpath level (G.convert $ getIxD ds1, G.convert $ getIxD dss1)
  updatePath mpath level (U.empty,                G.convert $ getIxD dss2)
  treeSS1L <- mkTree cfg gen (level+1) mpath (getIxP ss1l)
  treeSS1U <- mkTree cfg gen (level+1) mpath (getIxP ss1u)
  treeSS2L <- mkTree cfg gen (level+1) mpath (getIxP ss2l)
  treeSS2U <- mkTree cfg gen (level+1) mpath (getIxP ss2u)
  return $ VpNode
        { nodeSS1L = treeSS1L
        , nodeMa   = ma
        , nodeSS1U = treeSS1U
        , nodeMb   = mb
        , nodeSS2L = treeSS2L
        , nodeMc   = mc
        , nodeSS2U = treeSS2U
        , nodeVP1  = vp1
        , nodeVP2  = vp2
        , nodeIx1  = ix1
        , nodeIx2  = ix2
        }
  where
    getIxP = G.map (\(a, b, _) -> (a, b))
    getIxD = G.map (\(a, _, c) -> (a, c))

    ((ix1, vp1), ps1) = popRandom gen points
    ds1 = G.map (\(i, q) -> (i, q, dist vp1 q)) ps1
    (ds1l, ds1u, mb) = cutMedian ds1

    -- ((ix2, vp2, _), ps2) = popRandom gen ds1u
    ((ix2, vp2, _), ps2) = popAt (G.length ds1u - 1) ds1u
    dss1 = G.map (\(i, q, _) -> (i, q, dist vp2 q)) ds1l
    dss2 = G.map (\(i, q, _) -> (i, q, dist vp2 q)) ps2
    (ss1l, ss1u, ma) = cutMedian dss1
    (ss2l, ss2u, mc) = cutMedian dss2

genLeaf :: ( G.Vector v (Int, p), G.Vector v (Int, p, Double)
           , G.Vector v (Int, p, Double, Double), Metric p, U.Unbox p )
          => MVPCfg -> StdGen -> v (Int, p) -> VPtree p
genLeaf _ gen points =
  VpLeaf
  { leafVP1 = vp1
  , leafVP2 = vp2
  , leafIx1 = ix1
  , leafIx2 = ix2
  , leafPS  = psf
  }
  where
    ((ix1, vp1), ps1) = popRandom gen points
    ds1 = G.map (\(i, q) -> (i, q, dist vp1 q)) ps1

    ivp2 = G.maxIndexBy (compare `on` (\(_,_,d) -> d)) ds1
    ((ix2, vp2, _), ds2) = popAt ivp2 ds1
    psf = U.map (\(i, q, d1) -> (i, q, d1, dist vp2 q)) $ G.convert ds2


updatePath :: MVar Path -> Level -> (U.Vector (Int, Double), U.Vector (Int, Double)) -> IO ()
updatePath mpath level ds@(ds1, ds2) = do
  lpath <- fmap V.length (readMVar mpath)
  if level >= lpath
    then modifyMVar_ mpath (return . func1) >> updatePath mpath level ds
    else modifyMVar_ mpath (return . func2)
  where
    func1 path = V.snoc path (nullds, nullds)
      where
        size   = if V.null path then 0 else U.length (fst $ V.last path)
        nullds = U.replicate size 0
    func2 path = V.update path (V.singleton (level, (newds1, newds2)))
      where
        (oldds1, oldds2) = path V.! level
        newds1 = U.update oldds1 ds1
        newds2 = U.update oldds2 ds2

getPathAt :: Path -> Int -> Level -> (Double, Double)
getPathAt path level ix = let (xs1, xs2) = path V.! level in (xs1 U.! ix, xs2 U.! ix)



nearNeighbors :: (Metric p, U.Unbox p)
              => (VPtree p, Path) -> Double -> p -> [(Int, p, Double)]
nearNeighbors tree r q = nearNeighbors' tree V.empty r q

-- | Finds all near neighbors within r distance of the tree.
nearNeighbors' :: (Metric p, U.Unbox p)
               => (VPtree p, Path) -> V.Vector (Double, Double) -> Double -> p -> [(Int, p, Double)]
nearNeighbors' (VpEmpty, _) _ _ _ = []
nearNeighbors' (VpSingleLeaf vp ix, _) _ radius q
  | d <= radius = [(ix, vp, d)]
  | otherwise   = []
  where d = dist vp q
nearNeighbors' (VpLeaf{..}, path) hist radius q =
  addVP d1 leafVP1 leafIx1 $
  addVP d2 leafVP2 leafIx2 ss
  where
    d1 = dist leafVP1 q
    d2 = dist leafVP2 q
    addVP d vp ix xs = if d <= radius then (ix, vp, d) : xs else xs
    isInRange d ds = (d - radius <= ds && ds <= d + radius)
    test i level (p1, p2) = let
      (ps1, ps2) = getPathAt path level i
      in isInRange p1 ps1 && isInRange p2 ps2
    func (i, p, ds1, ds2)
      | not (isInRange d1 ds1) || not (isInRange d2 ds2) = Nothing
      | V.foldl' (&&) True (V.imap (test i) hist) = let
          dp = dist p q
          in if dp <= radius
             then Just (i, p, dp)
             else Nothing
      | otherwise = Nothing
    ss = G.foldl' (\acc x -> maybe acc (:acc) (func x)) [] leafPS
nearNeighbors' (VpNode{..}, path) hist radius q =
  go $ concatMap snd $ filter fst [test1, test2, test3, test4]
  where
    test1 = (d1 <= nodeMb + radius && d2 <= nodeMa + radius, deeperSS1L)
    test2 = (d1 <= nodeMb + radius && d2 >= nodeMa - radius, deeperSS1U)
    test3 = (d1 >= nodeMb - radius && d2 <= nodeMc + radius, deeperSS2L)
    test4 = (d1 >= nodeMb - radius && d2 >= nodeMc - radius, deeperSS2U)
    d1    = dist nodeVP1 q
    d2    = dist nodeVP2 q
    newhist = if V.length hist > 5 then hist else V.snoc hist (d1, d2)
    deeperSS1L = nearNeighbors' (nodeSS1L, path) newhist radius q
    deeperSS1U = nearNeighbors' (nodeSS1U, path) newhist radius q
    deeperSS2L = nearNeighbors' (nodeSS2L, path) newhist radius q
    deeperSS2U = nearNeighbors' (nodeSS2U, path) newhist radius q
    go = addVP d1 nodeVP1 nodeIx1 . addVP d2 nodeVP2 nodeIx2
    addVP d vp ix xs = if d <= radius then (ix, vp, d) : xs else xs

{--
    test1 = (d1 - radius <= nodeMb && d2 - radius <= nodeMa, deeperSS1L)
    test2 = (d1 - radius <= nodeMb && d2 + radius >= nodeMa, deeperSS1U)
    test3 = (d1 + radius >= nodeMb && d2 - radius <= nodeMc, deeperSS2L)
    test4 = (d1 + radius >= nodeMb && d2 + radius >= nodeMc, deeperSS2U)

--}
