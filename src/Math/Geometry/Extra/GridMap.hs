{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Math.Geometry.Extra.GridMap where

import Data.Extra.Tuple (mapFst, maybeSnd)
import Data.Function ((&))
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Text (pack, splitOn, unpack)
import Linear (V2 (..))
import qualified Math.Geometry.Grid as G
import Math.Geometry.Grid.Octagonal (RectOctGrid, rectOctGrid)
import Math.Geometry.GridMap (GridMap (..))
import qualified Math.Geometry.GridMap as GM
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMapIndexed)

swap ::
  ( k1 ~ G.Index (BaseGrid gm v),
    k2 ~ G.Index (BaseGrid gm v),
    Ord k1,
    Ord k2,
    GridMap gm v
  ) =>
  k1 ->
  k2 ->
  gm v ->
  gm v
swap k1 k2 gm =
  let v1 = GM.lookup k1 gm
      v2 = GM.lookup k2 gm
   in gm & GM.alter (const v1) k2 & GM.alter (const v2) k1

vecToKey :: V2 Int -> (Int, Int)
vecToKey (V2 x y) = (x, y)

keyToVec :: (Int, Int) -> V2 Int
keyToVec (x, y) = V2 x y

toAscii :: (Maybe v -> Char) -> LGridMap RectOctGrid v -> String
toAscii vToC gm =
  let (w, h) = G.size (GM.toGrid gm)
      asciiArray = fmap vToC <$> [[GM.lookup (x, y) gm | x <- [0 .. (w - 1)]] | y <- [0 .. (h - 1)]]
   in intercalate "\n" asciiArray

fromAscii :: (Char -> Maybe v) -> String -> LGridMap RectOctGrid v
fromAscii cToV s =
  let asciiArray = zip (splitOn "\n" (pack s)) [(0 :: Int) ..]
      valueArray = mapFst (zip [(0 :: Int) ..] . fmap cToV . unpack) <$> asciiArray
      indexed = mapMaybe maybeSnd ((\(vs, y) -> (\(x, v) -> ((x, y), v)) <$> vs) =<< valueArray)
      h = length asciiArray
      w = length $ unpack $ fst $ head asciiArray
   in lazyGridMapIndexed (rectOctGrid w h) indexed