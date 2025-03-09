{-# LANGUAGE TypeOperators #-}

module Math.Geometry.Extra.GridMap where

import Data.Function ((&))
import qualified Math.Geometry.Grid as G
import Math.Geometry.GridMap (GridMap (..))
import qualified Math.Geometry.GridMap as GM

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