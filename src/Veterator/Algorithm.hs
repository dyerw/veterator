{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Veterator.Algorithm where

import Control.Applicative (liftA2)
import Control.Monad (forM)
import Control.Monad.Random (when)
import Control.Monad.State (State, evalState, gets, modify)
import Data.Extra.Tuple (toFst)
import qualified Data.IntervalSet as IS
import Data.Map (Map, member, (!))
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.PQueue.Prio.Min (MinPQueue (..))
import qualified Data.PQueue.Prio.Min as PQ
import Linear (V2 (V2))
import Math.Geometry.Grid (Index, distance, neighbours)
import Math.Geometry.Grid.Octagonal (RectOctGrid)
import Math.Geometry.GridMap (GridMap (BaseGrid, toGrid))
import qualified Math.Geometry.GridMap as GM
import qualified Math.Geometry.GridMap.Lazy as LGM

(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&&) = liftA2 (&&)

data Blocked = Blocked deriving (Show)

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

data Visibility = Visible | Obstructed deriving (Show, Eq)

isVisible :: Visibility -> Bool
isVisible Visible = True
isVisible _ = False

type Shadow = IS.Interval Double

type Shadows = IS.IntervalSet Double

projectTile :: V2 Int -> Shadow
projectTile (V2 row col) =
  IS.Interval
    (fromIntegral col / fromIntegral (row + 2))
    (fromIntegral (col + 1) / fromIntegral (row + 1))

tileVisibility :: V2 Int -> Shadows -> Visibility
tileVisibility t shadows = if shadows `IS.subsumes` projectTile t then Obstructed else Visible

shadowCastTile :: V2 Int -> Maybe Blocked -> State Shadows Visibility
shadowCastTile tile block = do
  visibility <- gets (tileVisibility tile) -- before updating shadows
  when
    (isJust block)
    (modify (`IS.insert` projectTile tile))
  pure visibility

shadowCastRow :: Int -> [Maybe Blocked] -> State Shadows [Visibility]
shadowCastRow row blocks = do
  forM (zip [0 ..] blocks) (\(col, bs) -> shadowCastTile (V2 row col) bs)

shadowCastOctant :: [[Maybe Blocked]] -> [[Visibility]]
shadowCastOctant blocks = evalState shadowCastOctantM IS.empty
  where
    shadowCastOctantM = do
      forM (zip [0 ..] blocks) (uncurry shadowCastRow)

recursiveShadowCastFOV :: LGM.LGridMap RectOctGrid Blocked -> V2 Int -> Int -> LGM.LGridMap RectOctGrid Visibility
recursiveShadowCastFOV gm (V2 viewerX viewerY) dist = insertVisibilities $ Obstructed <$ gm
  where
    octantPositions = [[(row + viewerX, col + viewerY) | col <- [0 .. row]] | row <- [0 .. dist]]
    blockedOctant = fmap (`GM.lookup` gm) <$> octantPositions
    octantShadows = shadowCastOctant blockedOctant

    insertVisibilities :: LGM.LGridMap RectOctGrid Visibility -> LGM.LGridMap RectOctGrid Visibility
    insertVisibilities =
      foldr
        (.)
        id
        [ GM.insert (row + viewerX, col + viewerY) v
          | (row, vs) <- zip [0 ..] octantShadows,
            (col, v) <- zip [0 ..] vs
        ]