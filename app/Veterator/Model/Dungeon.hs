{-# LANGUAGE NamedFieldPuns #-}

module Veterator.Model.Dungeon where

import Data.Foldable (find)
import Data.Function ((&))
import Data.UUID (UUID)
import Linear (V2 (V2))
import Linear.Affine (Point (P))
import Math.Geometry.Extra.GridMap (swap)
import Math.Geometry.Grid (Grid (Index, contains))
import Math.Geometry.Grid.Octagonal (RectOctGrid)
import Math.Geometry.GridMap (GridMap (toList), (!))
import qualified Math.Geometry.GridMap as GM
import Math.Geometry.GridMap.Lazy (LGridMap)
import Veterator.Model.Creature (Creature (creatureId), Item, isAlive)

data Dungeon = Dungeon
  { dungeonTiles :: Tiles,
    dungeonCreatures :: Creatures,
    dungeonItems :: Items
  }

type DungeonPosition = Index RectOctGrid

type Tiles = LGridMap RectOctGrid Tile

data Tile = Floor | Wall | StairUp | StairDown deriving (Eq)

type Creatures = LGridMap RectOctGrid Creature

type Items = LGridMap RectOctGrid [Item]

toPoint :: DungeonPosition -> Point V2 Int
toPoint (x, y) = P (V2 x y)

fromPoint :: Point V2 Int -> DungeonPosition
fromPoint (P (V2 x y)) = (x, y)

findTiles :: Tile -> Tiles -> [DungeonPosition]
findTiles t tiles = tiles & toList & filter ((== t) . snd) & map fst

dungeonFindTiles :: Tile -> Dungeon -> [DungeonPosition]
dungeonFindTiles t d = findTiles t (dungeonTiles d)

emptyTiles :: Tiles -> [DungeonPosition]
emptyTiles = findTiles Floor

dungeonEmptyTiles :: Dungeon -> [DungeonPosition]
dungeonEmptyTiles = emptyTiles . dungeonTiles

isEmpty :: Dungeon -> DungeonPosition -> Bool
isEmpty Dungeon {dungeonTiles} i = (dungeonTiles ! i) == Floor

inBounds :: Dungeon -> DungeonPosition -> Bool
inBounds Dungeon {dungeonTiles} = contains dungeonTiles

getAllCreatures :: Dungeon -> [Creature]
getAllCreatures = GM.elems . dungeonCreatures

getCreatureAt :: Dungeon -> DungeonPosition -> Maybe Creature
getCreatureAt Dungeon {dungeonCreatures} p = GM.lookup p dungeonCreatures

getCreatureWithPosition :: Dungeon -> UUID -> Maybe (DungeonPosition, Creature)
getCreatureWithPosition Dungeon {dungeonCreatures} uuid = find ((== uuid) . creatureId . snd) (toList dungeonCreatures)

getCreature :: Dungeon -> UUID -> Maybe Creature
getCreature d uuid = snd <$> getCreatureWithPosition d uuid

getCreaturePosition :: Dungeon -> UUID -> Maybe DungeonPosition
getCreaturePosition d uuid = fst <$> getCreatureWithPosition d uuid

updateCreature :: Dungeon -> UUID -> (Creature -> Creature) -> Dungeon
updateCreature d uuid f =
  d
    { dungeonCreatures = maybe creatures (\p -> GM.adjust f p creatures) position
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
      Nothing -> d {dungeonCreatures = swap destination origin (dungeonCreatures d)}
    Nothing -> d

-- FIXME: Ok now we're really asking for a State monad
removeDeadCreatures :: Dungeon -> ([Creature], Dungeon)
removeDeadCreatures dungeon =
  let creatures = dungeonCreatures dungeon
      nextCreatures = GM.filter isAlive creatures
   in (filter (not . isAlive) (GM.elems creatures), dungeon {dungeonCreatures = nextCreatures})