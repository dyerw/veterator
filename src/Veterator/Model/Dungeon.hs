{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Veterator.Model.Dungeon where

import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Extra.Tuple (mapFst)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.UUID (UUID)
import Linear (Additive ((^+^), (^-^)), V2 (V2))
import Math.Geometry.Extra.GridMap (posMap, swap)
import Math.Geometry.Grid (Grid (Index))
import Math.Geometry.Grid.Octagonal (RectOctGrid, UnboundedOctGrid, rectOctGrid)
import Math.Geometry.GridMap (GridMap (toList))
import qualified Math.Geometry.GridMap as GM
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMapIndexed)
import Veterator.Algorithm (Blocked (Blocked), raycastFOV)
import Veterator.Model.Creature (Creature (creatureId), Item, isAlive)

data Dungeon = Dungeon
  { dungeonTiles :: Tiles,
    dungeonCreatures :: Creatures,
    dungeonItems :: Items
  }

-- Dungeon position is in one uninterrupted coordinate space and can be decomposed
-- into a world position and a chunk position
type DungeonPosition = V2 Int

toTup :: V2 Int -> (Int, Int)
toTup (V2 x y) = (x, y)

fromTup :: (Int, Int) -> V2 Int
fromTup (x, y) = V2 x y

-- Chunk position is the coordinate of a tile within a chunk
type TilePosition = Index RectOctGrid

type ChunkPosition = Index RectOctGrid

-- Tile index is a coordinate in the grid of grids and then in
-- the sub-grid
type TileIndex = (ChunkPosition, TilePosition)

chunkSize :: Int
chunkSize = 100

dungeonToChunk :: Int -> (Int, Int)
dungeonToChunk i = (div i chunkSize, mod i chunkSize)

chunkToDungeon :: (Int, Int) -> Int
chunkToDungeon (c, d) = c * chunkSize + d

dungeonPosToTileIndex :: DungeonPosition -> TileIndex
dungeonPosToTileIndex =
  (\((cx, dx), (cy, dy)) -> ((cx, cy), (dx, dy)))
    . bimap dungeonToChunk dungeonToChunk
    . toTup

tileIndexToDungeonPos :: TileIndex -> DungeonPosition
tileIndexToDungeonPos =
  fromTup
    . bimap chunkToDungeon chunkToDungeon
    . (\((cx, cy), (dx, dy)) -> ((cx, dx), (cy, dy)))

type TileChunk = LGridMap RectOctGrid Tile

-- A TileSlice can cross chunk boundaries and be any width or height
type TileSlice = TileChunk

type Tiles = LGridMap UnboundedOctGrid TileChunk

data Tile = Floor | Wall | StairUp | StairDown deriving (Eq)

blocksLOS :: Tile -> Bool
blocksLOS = \case
  Wall -> True
  _ -> False

-- Since these are sparsely populated they're indexed by DungeonPosition
type Creatures = LGridMap UnboundedOctGrid Creature

type Items = LGridMap UnboundedOctGrid [Item]

findChunkTiles :: Tile -> ChunkPosition -> TileChunk -> [DungeonPosition]
findChunkTiles t chunkPos chunk =
  chunk
    & toList
    & filter ((== t) . snd)
    & map (tileIndexToDungeonPos . (chunkPos,) . fst)

emptyChunkTiles :: ChunkPosition -> TileChunk -> [DungeonPosition]
emptyChunkTiles = findChunkTiles Floor

findTiles :: Tile -> Tiles -> [DungeonPosition]
findTiles t tiles = toList tiles & map (uncurry (findChunkTiles t)) & join

dungeonFindTiles :: Tile -> Dungeon -> [DungeonPosition]
dungeonFindTiles t d = findTiles t (dungeonTiles d)

emptyTiles :: Tiles -> [DungeonPosition]
emptyTiles = findTiles Floor

dungeonEmptyTiles :: Dungeon -> [DungeonPosition]
dungeonEmptyTiles = emptyTiles . dungeonTiles

isEmpty :: Dungeon -> DungeonPosition -> Bool
isEmpty Dungeon {dungeonTiles} pos = getTile pos dungeonTiles == Floor

getTile :: DungeonPosition -> Tiles -> Tile
getTile pos tiles = tile
  where
    (ci, ti) = dungeonPosToTileIndex pos
    fnErrorInfo = "in getTile " ++ show pos ++ ": "
    chunk =
      fromMaybe (error $ fnErrorInfo ++ "Invalid chunk index: " ++ show ci ++ " in generated chunks: " ++ show (GM.keys tiles)) $
        GM.lookup ci tiles
    tile =
      fromMaybe (error $ fnErrorInfo ++ "Invalid tile index: " ++ show ti) $
        GM.lookup ti chunk

-- Return a rectangular slice of the dungeon tiles, errors if ungenerated
tileSection :: DungeonPosition -> Int -> Int -> Tiles -> [(DungeonPosition, Tile)]
tileSection (V2 x y) width height tiles =
  [ (pos, getTile pos tiles)
    | x' <- [0 .. (width - 1)],
      y' <- [0 .. (height - 1)],
      let pos = V2 (x + x') (y + y')
  ]

-- Returned with pos at (0, 0)
tileSlice :: DungeonPosition -> Int -> Int -> Tiles -> TileSlice
tileSlice pos w h tiles =
  lazyGridMapIndexed
    (rectOctGrid w h)
    (mapFst (toTup . (^-^ pos)) <$> tileSection pos w h tiles)

getAllCreatures :: Dungeon -> [Creature]
getAllCreatures = GM.elems . dungeonCreatures

getCreatureAt :: Dungeon -> DungeonPosition -> Maybe Creature
getCreatureAt Dungeon {dungeonCreatures} p = GM.lookup (toTup p) dungeonCreatures

getCreatureWithPosition :: Dungeon -> UUID -> Maybe (DungeonPosition, Creature)
getCreatureWithPosition Dungeon {dungeonCreatures} uuid =
  mapFst fromTup
    <$> find
      ((== uuid) . creatureId . snd)
      (toList dungeonCreatures)

getCreature :: Dungeon -> UUID -> Maybe Creature
getCreature d uuid = snd <$> getCreatureWithPosition d uuid

getCreaturePosition :: Dungeon -> UUID -> Maybe DungeonPosition
getCreaturePosition d uuid = fst <$> getCreatureWithPosition d uuid

updateCreature :: Dungeon -> UUID -> (Creature -> Creature) -> Dungeon
updateCreature d uuid f =
  d
    { dungeonCreatures = maybe creatures (\p -> GM.adjust f (toTup p) creatures) position
    }
  where
    creatures = dungeonCreatures d
    position = getCreaturePosition d uuid

-- | No-op if the space is occupied or uuid doesn't exist
moveCreature :: Dungeon -> UUID -> DungeonPosition -> Dungeon
moveCreature d uuid destination =
  case getCreatureWithPosition d uuid of
    Just (origin, _) -> case getCreatureAt d destination of
      Just _ -> d
      Nothing ->
        d
          { dungeonCreatures =
              swap
                (toTup destination)
                (toTup origin)
                (dungeonCreatures d)
          }
    Nothing -> d

-- FIXME: Ok now we're really asking for a State monad
removeDeadCreatures :: Dungeon -> ([Creature], Dungeon)
removeDeadCreatures dungeon =
  let creatures = dungeonCreatures dungeon
      nextCreatures = GM.filter isAlive creatures
   in (filter (not . isAlive) (GM.elems creatures), dungeon {dungeonCreatures = nextCreatures})

tilesVisibleFromPosition :: Dungeon -> DungeonPosition -> Int -> [DungeonPosition]
tilesVisibleFromPosition d pos radius =
  (^+^) topLeftOfSlice . fromTup <$> GM.keys visibilityMap
  where
    sliceSize = radius * 2
    sliceOffset = V2 (div sliceSize 2) (div sliceSize 2)
    topLeftOfSlice = pos ^-^ sliceOffset
    slice = tileSlice topLeftOfSlice sliceSize sliceSize (dungeonTiles d)
    blockedMap = posMap (const Blocked) $ GM.filter blocksLOS slice
    visibilityMap = raycastFOV blockedMap sliceOffset radius

tilesVisibleToCreature :: Dungeon -> UUID -> Int -> [DungeonPosition]
tilesVisibleToCreature d uuid radius = case getCreaturePosition d uuid of
  Just position -> tilesVisibleFromPosition d position radius
  Nothing -> []