{-# LANGUAGE TupleSections #-}

module Veterator.AlgorithmSpec (spec) where

import Control.Monad (forM_)
import Data.List (intercalate)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Math.Geometry.Grid (Grid (neighbours))
import Math.Geometry.Grid.Octagonal (UnboundedOctGrid (UnboundedOctGrid), rectOctGrid)
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMapIndexed)
import Test.Hspec
import Test.Hspec.Hedgehog
import Veterator.Algorithm (Blocked (Blocked), Visibility (..), findPathGDFS, isVisible, shadowCastOctant)

emptyGridMap :: LGridMap UnboundedOctGrid v
emptyGridMap = lazyGridMapIndexed UnboundedOctGrid []

genPos :: Gen (Int, Int)
genPos = do
  x <- Gen.int (Range.linear (-100) 100)
  y <- Gen.int (Range.linear (-100) 100)
  return (x, y)

genGrid :: Gen (LGridMap UnboundedOctGrid Blocked)
genGrid = do
  let genEntry = do
        pos <- genPos
        return (pos, Blocked)
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
      let gm = lazyGridMapIndexed (rectOctGrid 10 10) ((,Blocked) <$> [(4, 9), (4, 8), (4, 7), (4, 6), (4, 5)])
      let from = (0, 9)
      let to = (7, 9)
      let path = findPathGDFS gm from to
      path `shouldBe` [(1, 8), (2, 7), (3, 6), (3, 5), (4, 4), (5, 5), (6, 6), (7, 7), (8, 8), (7, 9)]

  describe "recursiveShadowCastFOV" $ do
    let asciiVisibilityOctant oct =
          intercalate "\n" $
            reverse $
              fmap (\v -> if isVisible v then '.' else 'X') <$> oct

    describe "shadowCastOctant" $ do
      it "returns an entirely unblocked octant as entirely visible" $ do
        let octant =
              [ [Nothing],
                [Nothing, Nothing],
                [Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing]
              ]

        let result =
              [ [Visible],
                [Visible, Visible],
                [Visible, Visible, Visible],
                [Visible, Visible, Visible, Visible]
              ]
        shadowCastOctant octant `shouldBe` result

      it "returns entirely blocked octant as entirey obstructed, except origin" $ do
        let octant =
              [ [Just Blocked],
                [Just Blocked, Just Blocked],
                [Just Blocked, Just Blocked, Just Blocked],
                [Just Blocked, Just Blocked, Just Blocked, Just Blocked]
              ]
        let result =
              [ [Visible],
                [Obstructed, Obstructed],
                [Obstructed, Obstructed, Obstructed],
                [Obstructed, Obstructed, Obstructed, Obstructed]
              ]
        shadowCastOctant octant `shouldBe` result

    it "unblocked walls are visible" $ do
      let octant = [[Nothing], [Just Blocked, Nothing]]
      let result = [[Visible], [Visible, Visible]]
      asciiVisibilityOctant (shadowCastOctant octant) `shouldBe` asciiVisibilityOctant result

    it "a single blocked tile directly in front casts an 1/2 sloped shadow" $ do
      let octant =
            [ [Nothing],
              [Just Blocked, Nothing],
              replicate 3 Nothing,
              replicate 4 Nothing,
              replicate 5 Nothing,
              replicate 6 Nothing,
              replicate 7 Nothing,
              replicate 8 Nothing
            ]
      let result =
            [ [Visible],
              [Visible, Visible],
              [Obstructed] <> replicate 2 Visible,
              replicate 2 Obstructed <> replicate 2 Visible,
              replicate 2 Obstructed <> replicate 3 Visible,
              replicate 3 Obstructed <> replicate 3 Visible,
              replicate 3 Obstructed <> replicate 4 Visible,
              replicate 4 Obstructed <> replicate 4 Visible
            ]
      asciiVisibilityOctant (shadowCastOctant octant) `shouldBe` asciiVisibilityOctant result