module Data.TreeToolsSpec (spec) where

import qualified Data.List as L
import qualified Data.Vector as V
import Test.Hspec
import Test.QuickCheck

import Data.Common
import Data.TreeTools

spec :: Spec
spec = do
    describe "TreeTools" $ do
        it "popAt removes the element at the given index" $
            property $
                \(xsList, i) ->
                    not (null xsList) ==> do
                        let xs = V.fromList (xsList :: [Int])
                        let idx = abs i `mod` length xsList
                        let (val, rest) = popAt idx xs
                        val `shouldBe` (xs V.! idx)
                        V.length rest `shouldBe` (V.length xs - 1)
                        L.sort (val : V.toList rest) `shouldBe` L.sort xsList

        it "cutMedian splits the vector at the median distance" $
            property $
                \psList ->
                    not (null psList) ==> do
                        let ds = V.fromList $ map (\(i, p, d) -> (i, p :: V3, abs d)) psList
                        let (lower, upper, median) = cutMedian ds
                        V.length lower + V.length upper `shouldBe` V.length ds
                        let getD (_, _, d) = d
                        V.all (\x -> getD x <= median) lower `shouldBe` True
                        V.all (\x -> getD x >= median) upper `shouldBe` True
                        let sorted = L.sortOn (\(_, _, d) -> d) (V.toList ds)
                        let i = length sorted `div` 2
                        let expectedMedian =
                                if i == 0
                                    then getD (sorted !! 0)
                                    else
                                        if i == length sorted
                                            then getD (sorted !! (length sorted - 1))
                                            else 0.5 * (getD (sorted !! (i - 1)) + getD (sorted !! i))
                        median `shouldBe` expectedMedian
