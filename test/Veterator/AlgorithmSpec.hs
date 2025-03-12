{-# LANGUAGE TupleSections #-}

module Veterator.AlgorithmSpec (spec) where

import Control.Monad (forM_)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Math.Geometry.Grid (Grid (neighbours))
import Math.Geometry.Grid.Octagonal (UnboundedOctGrid (UnboundedOctGrid), rectOctGrid)
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMapIndexed)
import Test.Hspec
import Test.Hspec.Hedgehog
import Veterator.Algorithm (findPathGDFS)

emptyGridMap :: LGridMap UnboundedOctGrid v
emptyGridMap = lazyGridMapIndexed UnboundedOctGrid []

genPos :: Gen (Int, Int)
genPos = do
  x <- Gen.int (Range.linear (-100) 100)
  y <- Gen.int (Range.linear (-100) 100)
  return (x, y)

genGrid :: Gen (LGridMap UnboundedOctGrid Bool)
genGrid = do
  let genEntry = do
        pos <- genPos
        b <- Gen.bool
        return (pos, b)
  l <- Gen.list (Range.linear 0 100) genEntry
  return $ lazyGridMapIndexed UnboundedOctGrid l

spec :: Spec
spec = describe "Veterator.Algorithm" $ do
  describe "findPathGDFS" $ do
    it "returns [] when from == to" $ hedgehog $ do
      pos <- forAll genPos
      grid <- forAll genGrid
      findPathGDFS grid pos pos === []
    it "returns [from] when from is adjacent to to" $ hedgehog $ do
      pos <- forAll genPos
      forM_
        (neighbours emptyGridMap pos)
        (\n -> findPathGDFS emptyGridMap pos n === [n])
    it "returns a straight diagonal path in an empty map" $ do
      let path = findPathGDFS emptyGridMap (0, 0) (5, 5)
      path `shouldBe` [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)]
    it "walks around a little wall" $ do
      --   0 1 2 3 4 5 6 7 8 9
      -- 0|. . . . . . . . . .
      -- 1|. . . . . . . . . .
      -- 2|. . . . . . . . . .
      -- 3|. . . . . . . . . .
      -- 4|. . . . * . . . . .
      -- 5|. . . * # * . . . .
      -- 6|. . . * # . * . . .
      -- 7|. . * . # . . * . .
      -- 8|. * . . # . . * . .
      -- 9|O . . . # . . X . .
      let gm = lazyGridMapIndexed (rectOctGrid 10 10) ((,False) <$> [(4, 9), (4, 8), (4, 7), (4, 6), (4, 5)])
      let from = (0, 9)
      let to = (7, 9)
      let path = findPathGDFS gm from to
      path `shouldBe` [(1, 8), (2, 7), (3, 6), (3, 5), (4, 4), (5, 5), (6, 6), (7, 7), (8, 8), (7, 9)]
