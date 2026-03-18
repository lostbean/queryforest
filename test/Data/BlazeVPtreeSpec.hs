module Data.BlazeVPtreeSpec (spec) where

import Data.Function (on)
import qualified Data.List as L
import qualified Data.Vector.Unboxed as U
import Test.Hspec
import Test.QuickCheck

import qualified Data.BlazeVPtree as BVP
import Data.Common

spec :: Spec
spec = do
    describe "BlazeVPtree" $ do
        it "finds all near neighbors correctly" $
            property $
                \(psList, q, radius) -> do
                    let ps = U.fromList (psList :: [V3])
                    let tree = BVP.fromVector ps
                    let results = BVP.nearNeighbors tree (abs radius) q
                    let bfResults = U.filter ((<= abs radius) . BVP.dist q . snd) (U.imap (,) ps)
                    let resultsSorted = L.sort $ map (\(i, _, _) -> i) results
                    let bfSorted = L.sort $ map fst $ U.toList bfResults
                    resultsSorted `shouldBe` bfSorted

        it "finds the nearest neighbor correctly" $
            property $
                \(psList, q) ->
                    not (null psList) ==> do
                        let ps = U.fromList (psList :: [V3])
                        let tree = BVP.fromVector ps
                        let result = BVP.nearestNeighbor tree q
                        let bfResult = U.minimumBy (compare `on` (BVP.dist q . snd)) (U.imap (,) ps)
                        case result of
                            Nothing -> expectationFailure "Should have found a nearest neighbor"
                            Just (ix, _, d) -> do
                                ix `shouldBe` fst bfResult
                                d `shouldBe` BVP.dist q (snd bfResult)

        it "nearestThanNeighbor returns same result as nearestNeighbor" $
            property $
                \(psList, q) ->
                    not (null psList) ==> do
                        let ps = U.fromList (psList :: [V3])
                        let tree = BVP.fromVector ps
                        let dInit = 1000.0 -- some large value
                        BVP.nearestThanNeighbor tree dInit q `shouldBe` BVP.nearestNeighbor tree q
