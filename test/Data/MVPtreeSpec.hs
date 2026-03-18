module Data.MVPtreeSpec (spec) where

import Data.Function (on)
import qualified Data.List as L
import qualified Data.Vector.Unboxed as U
import Test.Hspec
import Test.QuickCheck

import Data.Common
import qualified Data.MVPtree as MVP

spec :: Spec
spec = do
    describe "MVPtree" $ do
        it "finds all near neighbors correctly" $
            property $
                \(psList, q, radius) -> do
                    let ps = U.fromList (psList :: [V3])
                    -- MVP.fromVector returns IO
                    (tree, path) <- MVP.fromVector ps
                    let results = MVP.nearNeighbors (tree, path) (abs radius) q
                    let bfResults = U.filter ((<= abs radius) . MVP.dist q . snd) (U.imap (,) ps)
                    let resultsSorted = L.sort $ map (\(i, _, _) -> i) results
                    let bfSorted = L.sort $ map fst $ U.toList bfResults
                    resultsSorted `shouldBe` bfSorted
