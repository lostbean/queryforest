{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Common (V3)
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Vector.Unboxed as U
import System.Random

import qualified Data.BlazeVPtree as BVP
import qualified Data.VPtree as VP

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
    t <- fmap (U.head . U.fromList . take 1) getDDD
    let
        mvp = BVP.fromVector rs
        vp = VP.fromVector rs
        d = (1.5 * pi / 180)

        vpo = VP.nearNeighbors vp d t
        vpnst = VP.nearestNeighbor vp t
        vplist = L.sort $ map (\(i, _, _) -> i) vpo

        mvpo = BVP.nearNeighbors mvp d t
        mvpnst = BVP.nearestNeighbor mvp t
        mvplist = L.sort $ map (\(i, _, _) -> i) mvpo

        bfo = U.filter ((< d) . VP.dist t . snd) (U.imap (,) rs)
        bfnst = U.minimumBy (compare `on` (VP.dist t . snd)) bfo
        bflist = L.sort $ map fst $ U.toList bfo

        check l = do
            putStr "match brutal force: "
            if (bflist == l)
                then putStrLn "ok"
                else putStrLn "failed!" >> error "test failed :("

    putStrLn "\n==========Brutal Force=========="
    if (not $ U.null bfo) then (putStrLn $ "nst: " ++ show bfnst) else return ()
    putStrLn $ "ix: " ++ show bflist
    putStrLn $ "#: " ++ show (length bflist)

    putStrLn "=========VP tree==========="
    putStrLn $ "nst: " ++ show vpnst
    putStrLn $ "ix: " ++ show vplist
    putStrLn $ "#: " ++ show (length vplist)
    check vplist

    putStrLn "=========BVP tree==========="
    putStrLn $ "nst: " ++ show mvpnst
    putStrLn $ "ix: " ++ show mvplist
    putStrLn $ "#: " ++ show (length mvplist)
    check mvplist
