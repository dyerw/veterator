module Gen.Dungeon where

import Control.Monad.Random.Lazy (MonadRandom (..), Rand)
import Data.Extra.List (count, shuffle)
import Data.Function ((&))
import Data.UUID (UUID)
import Math.Geometry.Grid (Grid (neighbours))
import Math.Geometry.Grid.Octagonal (rectOctGrid)
import Math.Geometry.GridMap (GridMap (insert, mapWithKey, toGrid, (!)))
import Math.Geometry.GridMap.Lazy (empty, lazyGridMap, lazyGridMapIndexed)
import System.Random (RandomGen)
import Veterator.Model.Creature (Creature (..), CreatureStats (..), CreatureType (..))
import Veterator.Model.Dungeon (Creatures, Items, Tile (..), Tiles, emptyTiles)

data DungeonGeneration = DungeonGeneration
  { generationTiles :: Tiles,
    generationItems :: Items,
    generationCreatures :: Creatures,
    generationPlayerUUID :: UUID
  }

data GenOrigin = CaveOrigin | TombOrigin

randomOrigin :: (RandomGen g) => Rand g GenOrigin
randomOrigin = do
  n <- getRandomR (0 :: Int, 1 :: Int)
  pure $ case n of
    0 -> CaveOrigin
    _ -> TombOrigin

data GenCurrentUse = BanditHideout | CultHideout | GoblinDen | WizardsLaboratory

randomCurrentUse :: (RandomGen g) => Rand g GenCurrentUse
randomCurrentUse = do
  n <- getRandomR (0 :: Int, 3 :: Int)
  pure $ case n of
    0 -> BanditHideout
    1 -> CultHideout
    2 -> GoblinDen
    _ -> WizardsLaboratory

generateDungeon :: (RandomGen g) => Int -> Int -> Rand g DungeonGeneration
generateDungeon width height = do
  tiles <- generateTiles width height
  (creatureGrid, playerUUID) <- generateCreatures tiles
  pure
    DungeonGeneration
      { generationTiles = tiles,
        generationCreatures = creatureGrid,
        generationPlayerUUID = playerUUID,
        generationItems = empty (toGrid tiles)
      }

genUUIDs :: (RandomGen g) => Rand g [UUID]
genUUIDs = getRandoms

generateCreatures :: (RandomGen g) => Tiles -> Rand g (Creatures, UUID)
generateCreatures tiles = do
  spawnPositions <- shuffle (emptyTiles tiles)
  let validSpawns = length spawnPositions
  -- One spawn must be reserved for the player
  numMonsters <- min (validSpawns - 1) <$> getRandomR (10 :: Int, 20 :: Int)
  uuids <- genUUIDs
  let playerUUID = head uuids
  let goblins = take numMonsters $ mkGoblin <$> tail uuids
  pure
    ( lazyGridMapIndexed
        (toGrid tiles)
        (zip spawnPositions goblins)
        & insert (last spawnPositions) (mkPlayer playerUUID),
      playerUUID
    )
  where
    mkGoblin uuid =
      Creature
        { creatureId = uuid,
          creatureType = Goblin,
          creatureHealth = 10,
          creatureStats = CreatureStats {statsMaxHealth = 10, statsDamageRange = (1, 3)},
          creatureInventory = []
        }
    mkPlayer uuid =
      Creature
        { creatureId = uuid,
          creatureType = Adventurer,
          creatureHealth = 20,
          creatureStats = CreatureStats {statsMaxHealth = 20, statsDamageRange = (2, 8)},
          creatureInventory = []
        }

generateTiles :: (RandomGen g) => Int -> Int -> Rand g Tiles
generateTiles width height = do
  rolls <- getRandomRs (0 :: Float, 1 :: Float)
  let dungeonGrid = lazyGridMap (rectOctGrid width height) (floatToTile <$> rolls)
  pure $ nTimes 5 caveAutomataStep dungeonGrid
  where
    caveAutomataStep :: Tiles -> Tiles
    caveAutomataStep g =
      mapWithKey
        ( \i t ->
            let ns = neighbours g i
                wallCount = count (Wall ==) (t : ((g !) <$> ns))
             in if fromIntegral wallCount / fromIntegral (length ns) > (0.5 :: Double)
                  then Wall
                  else Floor
        )
        g

    nTimes :: Int -> (a -> a) -> (a -> a)
    nTimes 0 _ = id
    nTimes 1 f = f
    nTimes n f = f . nTimes (n - 1) f

    floatToTile :: Float -> Tile
    floatToTile f = if f < 0.45 then Wall else Floor
