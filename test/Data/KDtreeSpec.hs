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
                    let r = abs radius
                    let results = KDT.nearNeighbors tree r q
                    let bfResults = filter (\(_, p) -> KDT.dist q p <= r) (zip [0 ..] psList)
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
            -- q = (0.04, 0.0, 0.0)
            -- dist q (0,0,0) = 0.04. (Index 0)
            -- dist q (0.1,0,0) = 0.06. (Index 1)
            -- nearest is (0,0,0) with index 0.
            let result = KDT.nearestNeighbor tree (0.04, 0.0, 0.0)
            fmap fst result `shouldBe` Just 0
