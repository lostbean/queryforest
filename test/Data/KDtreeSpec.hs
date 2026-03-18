module Data.KDtreeSpec (spec) where

import Data.Function (on)
import qualified Data.List as L
import Data.Maybe
import qualified Data.Vector as V
import Test.Hspec
import Test.QuickCheck

import Data.Common
import qualified Data.KDtree as KDT

spec :: Spec
spec = do
    describe "KDtree" $ do
        it "finds all near neighbors correctly" $
            property $
                \(psList, q, radius) -> do
                    let ps = V.fromList (psList :: [V3])
                    let tree = KDT.fromVector ps
                    let r2 = (abs radius) ** 2
                    let results = KDT.nearNeighbors tree r2 q
                    let bfResults = filter (\(_, p) -> KDT.dist q p <= r2) (zip [0 ..] psList)
                    let resultsSorted = L.sort $ map (\(i, _, _) -> i) results
                    let bfSorted = L.sort $ map fst bfResults
                    resultsSorted `shouldBe` bfSorted

        it "finds the nearest neighbor correctly" $
            property $
                \(psList, q) ->
                    not (null psList) ==> do
                        let ps = V.fromList (psList :: [V3])
                        let tree = KDT.fromVector ps
                        let result = KDT.nearestNeighbor tree q
                        let bfResult = L.minimumBy (compare `on` (KDT.dist q . snd)) (zip [0 ..] psList)
                        case result of
                            Nothing -> expectationFailure "Should have found a nearest neighbor"
                            Just (ix, _) -> do
                                ix `shouldBe` fst bfResult

        it "reproducibly finds the nearest neighbor with small distances" $ do
            let ps = V.fromList [(0.0, 0.0, 0.0), (0.1, 0.0, 0.0)] :: V.Vector V3
            let tree = KDT.fromVector ps
            let q = (0.06, 0.0, 0.0)
            -- dist q (0,0,0) = 0.06^2 = 0.0036
            -- dist q (0.1,0,0) = 0.04^2 = 0.0016
            -- nearest is (0.1,0,0) with index 1.
            -- if xProbe <= xp: 0.06 <= 0.0 is False.
            -- findNearest r l: tree1=r, tree2=l.
            -- nearestNeighbor r q: Just (1, (0.1,0,0))
            -- best1 = (1, (0.1,0,0)), d = 0.0016
            -- xProbe = 0.06, xp = 0.0. abs(0.06 - 0.0) = 0.06.
            -- sphereIntersectsPlane = 0.06 <= 0.0016 which is False!
            -- but (0.06)^2 = 0.0036. Wait, the plane is at x=0.
            -- the distance to plane is 0.06, squared distance is 0.0036.
            -- current best d is 0.0016. 0.0036 > 0.0016, so it doesn't intersect. Correct.

            -- Let's try another one.
            -- q = (0.04, 0.0, 0.0)
            -- dist q (0,0,0) = 0.04^2 = 0.0016. (Index 0)
            -- dist q (0.1,0,0) = 0.06^2 = 0.0036.
            -- xProbe = 0.04, xp = 0.0.
            -- 0.04 <= 0.0 is False.
            -- findNearest r l: tree1=r, tree2=l.
            -- nearestNeighbor r q: Just (1, (0.1,0,0))
            -- best1 = (1, (0.1,0,0)), d = 0.0036.
            -- xProbe = 0.04, xp = 0.0. abs(0.04 - 0.0) = 0.04.
            -- sphereIntersectsPlane = 0.04 <= 0.0036 which is False!
            -- BUT it should be True if we use squared distance: 0.04^2 = 0.0016 <= 0.0036.
            -- Since it's False, it won't check the left branch (0,0,0), and will return (0.1,0,0).
            -- BUT (0,0,0) is closer!
            let result = KDT.nearestNeighbor tree (0.04, 0.0, 0.0)
            fmap fst result `shouldBe` Just 0
