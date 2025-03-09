{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Veterator.Algorithm where

import Data.Extra.Tuple (toFst)
import Data.Map (Map, member, (!))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min (MinPQueue (..), singleton)
import qualified Data.PQueue.Prio.Min as PQ
import Math.Geometry.Grid (Index, distance, neighbours)
import Math.Geometry.GridMap (GridMap (BaseGrid, toGrid))
import qualified Math.Geometry.GridMap as GM

-- | Lift boolean 'or' over predicates.
(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p ||| q = \v -> p v || q v

greedyDFSFindPath ::
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
greedyDFSFindPath g from to =
  traceFlow to [] $
    searchStep (singleton (0 :: Int) from) mempty
  where
    searchStep :: MinPQueue Int k -> Map k k -> Map k k
    searchStep Empty flowMap = flowMap
    searchStep ((_, current) :< pq) flowMap =
      if current == to
        then
          flowMap
        else
          let isVisited = (`member` flowMap)
              isBlocked = not . fromMaybe False . (`GM.lookup` g)
              unvisitedNeighbors = filter (isVisited ||| isBlocked) $ neighbours (toGrid g) current
              -- All neighbors come from current position
              nextFlowMap = foldl (flip (`M.insert` current)) flowMap unvisitedNeighbors
              -- Insert neighbors with distance heuristic as priority
              nextPQ =
                foldl
                  (flip (uncurry PQ.insert) :: MinPQueue Int k -> (Int, k) -> MinPQueue Int k)
                  pq
                  ((fmap . toFst) (distance (toGrid g) to) unvisitedNeighbors :: [(Int, k)])
           in searchStep nextPQ nextFlowMap
    traceFlow :: k -> [k] -> Map k k -> [k]
    traceFlow current path flowMap =
      if current == from
        then []
        else traceFlow (flowMap ! current) (current : path) flowMap

-- | https://www.roguebasin.com/index.php/Spiral_Path_FOV
spiralPathFOV :: ()
spiralPathFOV = undefined