module Data.VPtreeSpec (spec) where

import Data.Function (on)
import qualified Data.List as L
import qualified Data.Vector.Unboxed as U
import Test.Hspec
import Test.QuickCheck

import Data.Common
import qualified Data.VPtree as VP

spec :: Spec
spec = do
    describe "VPtree" $ do
        it "finds all near neighbors correctly" $
            property $
                \(psList, q, radius) -> do
                    let ps = U.fromList (psList :: [V3])
                    let tree = VP.fromVector ps
                    let results = VP.nearNeighbors tree (abs radius) q
                    let bfResults = U.filter ((<= abs radius) . VP.dist q . snd) (U.imap (,) ps)
                    let resultsSorted = L.sort $ map (\(i, _, _) -> i) results
                    let bfSorted = L.sort $ map fst $ U.toList bfResults
                    resultsSorted `shouldBe` bfSorted

        it "finds the nearest neighbor correctly" $
            property $
                \(psList, q) ->
                    not (null psList) ==> do
                        let ps = U.fromList (psList :: [V3])
                        let tree = VP.fromVector ps
                        let result = VP.nearestNeighbor tree q
                        let bfResult = U.minimumBy (compare `on` (VP.dist q . snd)) (U.imap (,) ps)
                        case result of
                            Nothing -> expectationFailure "Should have found a nearest neighbor"
                            Just (ix, _, d) -> do
                                ix `shouldBe` fst bfResult
                                d `shouldBe` VP.dist q (snd bfResult)

        it "toList contains all points" $
            property $
                \psList -> do
                    let ps = U.fromList (psList :: [V3])
                    let tree = VP.fromVector ps
                    let list = VP.toList tree
                    L.sort (map fst list) `shouldBe` L.sort [0 .. length psList - 1]

        it "flatTree contains all points" $
            property $
                \psList -> do
                    let ps = U.fromList (psList :: [V3])
                    let tree = VP.fromVector ps
                    let list = VP.flatTree tree
                    L.sort (map (\(i, _, _) -> i) list) `shouldBe` L.sort [0 .. length psList - 1]

        it "nearestThanNeighbor returns same result as nearestNeighbor" $
            property $
                \(psList, q) ->
                    not (null psList) ==> do
                        let ps = U.fromList (psList :: [V3])
                        let tree = VP.fromVector ps
                        let dInit = 1000.0 -- some large value
                        VP.nearestThanNeighbor tree dInit q `shouldBe` VP.nearestNeighbor tree q
