module Veterator.Model.Dungeon where

import Data.Foldable (find)
import Data.UUID (UUID)
import Linear (V2 (V2))
import Linear.Affine (Point (P))
import Math.Geometry.Extra.GridMap (swap)
import Math.Geometry.Grid (Grid (Index, contains))
import Math.Geometry.Grid.Octagonal (RectOctGrid)
import Math.Geometry.GridMap (GridMap (toList), (!))
import qualified Math.Geometry.GridMap as GM
import Math.Geometry.GridMap.Lazy (LGridMap)
import Veterator.Model.Creature (Creature, Item)

data Tile = Floor | Wall | StairUp | StairDown deriving (Eq)

type DungeonGrid = LGridMap RectOctGrid Tile

type CreatureGrid = LGridMap RectOctGrid (UUID, Creature)

type ItemsGrid = LGridMap RectOctGrid [Item]

type DungeonPosition = Index DungeonGrid

toPoint :: DungeonPosition -> Point V2 Int
toPoint (x, y) = P (V2 x y)

fromPoint :: Point V2 Int -> DungeonPosition
fromPoint (P (V2 x y)) = (x, y)

findTiles :: Tile -> DungeonGrid -> [DungeonPosition]
findTiles t = map fst . filter ((== t) . snd) . toList

emptyTiles :: DungeonGrid -> [DungeonPosition]
emptyTiles = findTiles Floor

isEmpty :: DungeonGrid -> DungeonPosition -> Bool
isEmpty g i = (g ! i) == Floor

inBounds :: DungeonGrid -> DungeonPosition -> Bool
inBounds = contains

getCreatureAt :: DungeonPosition -> CreatureGrid -> Maybe (UUID, Creature)
getCreatureAt = GM.lookup

getCreatureWithPosition :: CreatureGrid -> UUID -> Maybe (DungeonPosition, (UUID, Creature))
getCreatureWithPosition g uuid = find ((== uuid) . fst . snd) (toList g)

getCreaturePosition :: CreatureGrid -> UUID -> Maybe DungeonPosition
getCreaturePosition g uuid = fst <$> getCreatureWithPosition g uuid

-- Returns Nothing if the space is occupied
moveCreature :: UUID -> DungeonPosition -> CreatureGrid -> Maybe CreatureGrid
moveCreature uuid pos grid =
  case getCreatureWithPosition grid uuid of
    Just (originPos, _) -> case GM.lookup pos grid of
      Just _ -> Nothing
      Nothing -> Just $ swap pos originPos grid
    Nothing -> Nothing