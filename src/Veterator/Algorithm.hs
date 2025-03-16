{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Veterator.Algorithm where

import Control.Applicative (liftA2)
import Data.Extra.List (takeUntil)
import Data.Extra.Tuple (toFst)
import Data.Map (Map, member, (!))
import qualified Data.Map as M
import Data.PQueue.Prio.Min (MinPQueue (..))
import qualified Data.PQueue.Prio.Min as PQ
import Linear (Additive ((^+^)), V2 (V2))
import Math.Geometry.Extra.GridMap (vecToKey)
import Math.Geometry.Grid (Index, distance, neighbours)
import Math.Geometry.Grid.Octagonal (RectOctGrid)
import Math.Geometry.GridMap (GridMap (BaseGrid, toGrid))
import qualified Math.Geometry.GridMap as GM
import Math.Geometry.GridMap.Lazy (lazyGridMap)
import qualified Math.Geometry.GridMap.Lazy as LGM

(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&&) = liftA2 (&&)

data Blocked = Blocked deriving (Eq, Show)

isUnblocked :: forall gm k. (GridMap gm Blocked, k ~ Index (BaseGrid gm Blocked), Ord k) => gm Blocked -> k -> Bool
isUnblocked gm k = case GM.lookup k gm of
  Just Blocked -> False
  Nothing -> True

isBlocked :: forall gm k. (GridMap gm Blocked, k ~ Index (BaseGrid gm Blocked), Ord k) => gm Blocked -> k -> Bool
isBlocked gm = not . isUnblocked gm

-- A greedy depth first search using grid distance as the heuristic
-- This does not always find the shortest path but is faster and works
-- well for points that are visible to each other
findPathGDFS ::
  forall gm k.
  (GridMap gm Blocked, k ~ Index (BaseGrid gm Blocked), Eq k, Ord k) =>
  -- grid map containing spaces marked as blocked
  gm Blocked ->
  -- origin tile
  k ->
  -- destination tile
  k ->
  -- path to destination, not including origin tile
  [k]
findPathGDFS gm from to =
  traceFlow to [] $
    searchStep (PQ.singleton (0 :: Int) from) mempty
  where
    searchStep :: MinPQueue Int k -> Map k k -> Map k k
    searchStep Empty flowMap = flowMap
    searchStep ((_, current) :< pq) flowMap
      | current == to = flowMap
      | otherwise =
          let validNeighbors =
                filter (isUnblocked gm &&& isUnvisited flowMap) $
                  neighbours (toGrid gm) current
              -- All neighbors come from current position
              nextFlowMap = foldl (flip (`M.insert` current)) flowMap validNeighbors
              -- Insert neighbors with distance heuristic as priority
              nextPQ = PQ.union pq (PQ.fromList (withDistance validNeighbors))
           in searchStep nextPQ nextFlowMap

    traceFlow :: k -> [k] -> Map k k -> [k]
    traceFlow current path flowMap
      | current == from = path
      | otherwise = traceFlow (flowMap ! current) (current : path) flowMap

    isUnvisited :: Map k k -> k -> Bool
    isUnvisited flowMap = not . (`member` flowMap)

    withDistance :: [k] -> [(Int, k)]
    withDistance = (fmap . toFst) (distance (toGrid gm) to)

data Visibility = Visible deriving (Show, Eq)

boundingBox :: V2 Int -> Int -> [V2 Int]
boundingBox p d =
  (^+^) p
    <$> ( [V2 x y | x <- [(-d) .. d], y <- [-d, d]] -- Top and bottom
            <> [V2 x y | y <- [(-d + 1) .. d - 1], x <- [-d, d]] -- Sides
        )

line :: V2 Int -> V2 Int -> [V2 Int]
line (V2 startX startY) (V2 endX endY) =
  let dx = endX - startX
      dy = endY - startY
      step = max (abs dx) (abs dy)
      stepX = (fromIntegral dx / fromIntegral step) :: Double
      stepY = (fromIntegral dy / fromIntegral step) :: Double
   in [ V2 x y
        | i <- [0 .. (step + 1)],
          let x = round (fromIntegral startX + fromIntegral i * stepX),
          let y = round (fromIntegral startY + fromIntegral i * stepY)
      ]

euclideanDistance :: V2 Int -> V2 Int -> Double
euclideanDistance (V2 x0 y0) (V2 x1 y1) =
  sqrt
    ( sq (fromIntegral x1 - fromIntegral x0)
        + sq (fromIntegral y1 - fromIntegral y0)
    )
  where
    sq n = n * n

castLight :: V2 Int -> (V2 Int -> Bool) -> Int -> [V2 Int]
castLight source blocking radius = filter inRadius $ rays >>= takeUntil blocking
  where
    rays = line source <$> boundingBox source radius
    inRadius t = fromIntegral radius >= euclideanDistance source t

raycastFOV :: LGM.LGridMap RectOctGrid Blocked -> V2 Int -> Int -> LGM.LGridMap RectOctGrid Visibility
raycastFOV gm source range =
  foldr
    (\(V2 x y) gm' -> GM.insert (x, y) Visible gm')
    (lazyGridMap (toGrid gm) [])
    visibleTiles
  where
    visibleTiles = castLight source (isBlocked gm . vecToKey) range