{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Common (V3) where

import qualified Data.BlazeVPtree as BVP
import qualified Data.KDtree as KDT
import qualified Data.MVPtree as MVP
import qualified Data.VPtree as VP

type V3 = (Double, Double, Double)

instance VP.Metric V3 where
    dist (a1, a2, a3) (b1, b2, b3) =
        let
            x = b1 - a1
            y = b2 - a2
            z = b3 - a3
         in
            sqrt $ x * x + y * y + z * z

instance BVP.Metric V3 where
    dist = VP.dist

instance MVP.Metric V3 where
    dist = VP.dist

instance KDT.Point V3 where
    dimension _ = 3
    coord 0 (a, _, _) = a
    coord 1 (_, b, _) = b
    coord 2 (_, _, c) = c
    coord _ _ = error "Out of bounds"
