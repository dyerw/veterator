{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Veterator.Algorithm where

import Control.Applicative (liftA2)
import Data.Extra.Tuple (toFst)
import Data.Map (Map, member, (!))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min (MinPQueue (..))
import qualified Data.PQueue.Prio.Min as PQ
-- import Data.Set (Set)
import Math.Geometry.Grid (Index, distance, neighbours)
import Math.Geometry.GridMap (GridMap (BaseGrid, toGrid))
import qualified Math.Geometry.GridMap as GM

(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&&) = liftA2 (&&)

-- A greedy depth first search using grid distance as the heuristic
-- This does not always find the shortest path but is faster and works
-- well for points that are visible to each other
findPathGDFS ::
  forall gm k.
  (GridMap gm Bool, k ~ Index (BaseGrid gm Bool), Eq k, Ord k) =>
  -- | a GridMap where blocked tiles are False
  gm Bool ->
  -- | origin tile
  k ->
  -- | destination tile
  k ->
  -- | path to destination, not including origin tile
  [k]
findPathGDFS g from to =
  traceFlow to [] $
    searchStep (PQ.singleton (0 :: Int) from) mempty
  where
    searchStep :: MinPQueue Int k -> Map k k -> Map k k
    searchStep Empty flowMap = flowMap
    searchStep ((_, current) :< pq) flowMap
      | current == to = flowMap
      | otherwise =
          let validNeighbors =
                filter (isUnblocked &&& isUnvisited flowMap) $
                  neighbours (toGrid g) current
              -- All neighbors come from current position
              nextFlowMap = foldl (flip (`M.insert` current)) flowMap validNeighbors
              -- Insert neighbors with distance heuristic as priority
              nextPQ = PQ.union pq (PQ.fromList (withDistance validNeighbors))
           in searchStep nextPQ nextFlowMap

    traceFlow :: k -> [k] -> Map k k -> [k]
    traceFlow current path flowMap
      | current == from = path
      | otherwise = traceFlow (flowMap ! current) (current : path) flowMap

    isUnblocked :: k -> Bool
    isUnblocked = fromMaybe True . (`GM.lookup` g)

    isUnvisited :: Map k k -> k -> Bool
    isUnvisited flowMap = not . (`member` flowMap)

    withDistance :: [k] -> [(Int, k)]
    withDistance = (fmap . toFst) (distance (toGrid g) to)

-- | https://www.gridbugs.org/visible-area-detection-recursive-shadowcast/
-- recursiveShadowcastFOV ::
--   forall gm k.
--   (GridMap gm Bool, k ~ Index (BaseGrid gm Bool), Eq k, Ord k) =>
--   -- | a GridMap where blocked tiles are False
--   gm Bool ->
--   -- | viewpoint tile
--   k ->
--   -- | All visible tiles
--   Set k
-- recursiveShadowcastFOV gm viewpoint = undefined
--   where
--     castOctant :: Int -> Int -> Int -> Set k -> Set k
--     castOctant depth minSlope maxSlope visibleTiles = undefined