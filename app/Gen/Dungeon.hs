{-# LANGUAGE TupleSections #-}

module Gen.Dungeon where

import Control.Monad.Random.Lazy (MonadRandom (..), Rand)
import Data.Extra.List (count, shuffle)
import Data.Function ((&))
import Data.UUID (UUID)
import Math.Geometry.Grid (Grid (neighbours))
import Math.Geometry.Grid.Octagonal (rectOctGrid)
import Math.Geometry.GridMap (GridMap (insert, mapWithKey, toGrid, (!)))
import Math.Geometry.GridMap.Lazy (lazyGridMap, lazyGridMapIndexed)
import System.Random (RandomGen)
import Veterator.Model.Creature (Creature (..), CreatureStats (..), CreatureType (..))
import Veterator.Model.Dungeon (CreatureGrid, DungeonGrid, Tile (..), emptyTiles)

data DungeonGeneration = DungeonGeneration
  { generationTiles :: DungeonGrid,
    -- generationItems :: [(DungeonPosition, Item)],
    generationCreatures :: CreatureGrid,
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
        generationPlayerUUID = playerUUID
        --   items = []
      }

genUUIDs :: (RandomGen g) => Rand g [UUID]
genUUIDs = getRandoms

generateCreatures :: (RandomGen g) => DungeonGrid -> Rand g (CreatureGrid, UUID)
generateCreatures grid = do
  spawnPositions <- shuffle (emptyTiles grid)
  let validSpawns = length spawnPositions
  -- One spawn must be reserved for the player
  numMonsters <- min (validSpawns - 1) <$> getRandomR (10 :: Int, 20 :: Int)
  uuids <- genUUIDs
  let playerUUID = head uuids
  let goblin = Creature Goblin 10 (CreatureStats 10 1 3) []
  pure
    ( lazyGridMapIndexed
        (toGrid grid)
        ( uuids
            & tail -- leave the first for the player
            & fmap (,goblin)
            & zip spawnPositions
            & take numMonsters
        )
        & insert (last spawnPositions) (playerUUID, Creature Adventurer 20 (CreatureStats 20 2 8) []),
      playerUUID
    )

generateTiles :: (RandomGen g) => Int -> Int -> Rand g DungeonGrid
generateTiles width height = do
  rolls <- getRandomRs (0 :: Float, 1 :: Float)
  let dungeonGrid = lazyGridMap (rectOctGrid width height) (floatToTile <$> rolls)
  pure $ nTimes 5 caveAutomataStep dungeonGrid
  where
    caveAutomataStep :: DungeonGrid -> DungeonGrid
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
