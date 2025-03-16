{-# LANGUAGE TupleSections #-}

module Veterator.AlgorithmSpec (spec) where

import Control.Monad (forM_)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Linear (V2 (V2))
import qualified Math.Geometry.Extra.GridMap as GME
import Math.Geometry.Grid (Grid (neighbours), size)
import Math.Geometry.Grid.Octagonal (UnboundedOctGrid (UnboundedOctGrid), rectOctGrid)
import qualified Math.Geometry.GridMap as GM
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMapIndexed)
import Test.Hspec
import Test.Hspec.Hedgehog
import Veterator.Algorithm
  ( Blocked (Blocked),
    Visibility (..),
    boundingBox,
    findPathGDFS,
    raycastFOV,
  )

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

  describe "raycastFOV" $ do
    describe "boundingBox" $ do
      it "draws a bounding box around the origin" $ do
        let box = boundingBox (V2 0 0) 1
        sort box `shouldBe` sort [V2 (-1) (-1), V2 0 (-1), V2 1 (-1), V2 1 0, V2 1 1, V2 0 1, V2 (-1) 1, V2 (-1) 0]

    it "my debug functions make sense" $ do
      let ascii =
            "123\n"
              ++ "456\n"
              ++ "789"
      GME.toAscii (fromMaybe ' ') (GME.fromAscii Just ascii) `shouldBe` ascii

    it "can see with no blockers the correct range" $ do
      let blockers =
            "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ ".....@.....\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "..........."
      let visibility =
            "XXXXXXXXXXX\n"
              ++ "XXXXX.XXXXX\n"
              ++ "XXX.....XXX\n"
              ++ "XX.......XX\n"
              ++ "XX.......XX\n"
              ++ "X.........X\n"
              ++ "XX.......XX\n"
              ++ "XX.......XX\n"
              ++ "XXX.....XXX\n"
              ++ "XXXXX.XXXXX\n"
              ++ "XXXXXXXXXXX"

      let blockedGM = GME.fromAscii cToBlocked blockers
      let visibilityGM = raycastFOV blockedGM (V2 5 5) 4
      size (GM.toGrid visibilityGM) `shouldBe` size (GM.toGrid blockedGM)
      GME.toAscii visibilityToC visibilityGM `shouldBe` visibility

    it "can see the entire map" $ do
      let blockers =
            "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ ".....@.....\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "..........."
      let visibility =
            "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "..........."

      let blockedGM = GME.fromAscii cToBlocked blockers
      let visibilityGM = raycastFOV blockedGM (V2 5 5) 20
      size (GM.toGrid visibilityGM) `shouldBe` size (GM.toGrid blockedGM)
      GME.toAscii visibilityToC visibilityGM `shouldBe` visibility

    it "casts a vertical shadow" $ do
      let blockers =
            "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ ".....O.....\n"
              ++ ".....@.....\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "..........."
      let visibility =
            "...XXXXX...\n"
              ++ "....XXX....\n"
              ++ ".....X.....\n"
              ++ ".....X.....\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "...........\n"
              ++ "..........."

      let blockedGM = GME.fromAscii cToBlocked blockers
      let visibilityGM = raycastFOV blockedGM (V2 5 5) 20
      size (GM.toGrid visibilityGM) `shouldBe` size (GM.toGrid blockedGM)
      GME.toAscii visibilityToC visibilityGM `shouldBe` visibility
  it "can't see through walls" $ do
    let blockers =
          "...........\n"
            ++ "...........\n"
            ++ "...........\n"
            ++ "...........\n"
            ++ "OOOOOOOO...\n"
            ++ ".....@.O...\n"
            ++ ".......OOOO\n"
            ++ "...........\n"
            ++ "...........\n"
            ++ "...........\n"
            ++ "..........."
    let visibility =
          "XXXXXXXXXXX\n"
            ++ "XXXXXXXXXXX\n"
            ++ "XXXXXXXXXXX\n"
            ++ "XXXXXXXXXXX\n"
            ++ "........XXX\n"
            ++ "........XXX\n"
            ++ "........XXX\n"
            ++ ".........XX\n"
            ++ "..........X\n"
            ++ "...........\n"
            ++ "..........."

    let blockedGM = GME.fromAscii cToBlocked blockers
    let visibilityGM = raycastFOV blockedGM (V2 5 5) 20
    size (GM.toGrid visibilityGM) `shouldBe` size (GM.toGrid blockedGM)
    GME.toAscii visibilityToC visibilityGM `shouldBe` visibility
  where
    cToBlocked :: Char -> Maybe Blocked
    cToBlocked 'O' = Just Blocked
    cToBlocked _ = Nothing

    visibilityToC :: Maybe Visibility -> Char
    visibilityToC (Just Visible) = '.'
    visibilityToC Nothing = 'X'
