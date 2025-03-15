module Gen.Dungeon where

import Control.Monad.Random.Lazy (MonadRandom (..), Rand, foldM)
import Data.Extra.List (count, shuffle)
import Data.UUID (UUID)
import Math.Geometry.Grid (Grid (neighbours))
import Math.Geometry.Grid.Octagonal (UnboundedOctGrid (UnboundedOctGrid), rectOctGrid)
import Math.Geometry.GridMap (GridMap (mapWithKey, (!)), insert)
import qualified Math.Geometry.GridMap as GM
import Math.Geometry.GridMap.Lazy (empty, lazyGridMap, lazyGridMapIndexed)
import System.Random (RandomGen)
import Veterator.Model.Creature (Creature (..), CreatureStats (..), CreatureType (..))
import Veterator.Model.Dungeon (Creatures, DungeonPosition, Items, Tile (..), TileChunk, Tiles, chunkSize, emptyChunkTiles, toTup)

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

generateDungeon :: (RandomGen g) => Rand g DungeonGeneration
generateDungeon = do
  -- Generate chunk (0,0) and all of its neighbors
  initialChunk <- generateChunkTiles
  let chunks = lazyGridMapIndexed UnboundedOctGrid [((0, 0), initialChunk)]
  withNeighboringChunks <- fillNeighboringChunks chunks (0, 0)
  let spawnPositions = emptyChunkTiles (0, 0) initialChunk
  let playerSpawn = head spawnPositions
  playerUUID <- getRandom
  creatures <- insert (toTup playerSpawn) (mkPlayer playerUUID) <$> generateCreatures (tail spawnPositions)
  pure
    DungeonGeneration
      { generationTiles = withNeighboringChunks,
        generationCreatures = creatures,
        generationPlayerUUID = playerUUID,
        generationItems = empty UnboundedOctGrid
      }
  where
    mkPlayer uuid =
      Creature
        { creatureId = uuid,
          creatureType = Adventurer,
          creatureHealth = 20,
          creatureStats = CreatureStats {statsMaxHealth = 20, statsDamageRange = (2, 8)},
          creatureInventory = []
        }

genUUIDs :: (RandomGen g) => Rand g [UUID]
genUUIDs = getRandoms

generateCreatures :: (RandomGen g) => [DungeonPosition] -> Rand g Creatures
generateCreatures validSpawns = do
  numMonsters <- min (length validSpawns) <$> getRandomR (10 :: Int, 20 :: Int)
  spawnPositions <- take numMonsters <$> shuffle (toTup <$> validSpawns)
  uuids <- genUUIDs
  let goblins = mkGoblin <$> uuids
  pure $
    lazyGridMapIndexed
      UnboundedOctGrid
      (zip spawnPositions goblins)
  where
    mkGoblin uuid =
      Creature
        { creatureId = uuid,
          creatureType = Goblin,
          creatureHealth = 10,
          creatureStats = CreatureStats {statsMaxHealth = 10, statsDamageRange = (1, 3)},
          creatureInventory = []
        }

fillNeighboringChunks :: (RandomGen g) => Tiles -> (Int, Int) -> Rand g Tiles
fillNeighboringChunks tiles center = do
  foldM fillChunk tiles (neighbours tiles center)

-- TODO: Stitch the automata together
fillChunk :: (RandomGen g) => Tiles -> (Int, Int) -> Rand g Tiles
fillChunk tiles chunkPos = case GM.lookup chunkPos tiles of
  Nothing -> do
    newChunk <- generateChunkTiles
    pure $ insert chunkPos newChunk tiles
  Just _ -> pure tiles

generateChunkTiles :: (RandomGen g) => Rand g TileChunk
generateChunkTiles = do
  rolls <- getRandomRs (0 :: Float, 1 :: Float)
  let dungeonGrid = lazyGridMap (rectOctGrid chunkSize chunkSize) (floatToTile <$> rolls)
  pure $ nTimes 5 caveAutomataStep dungeonGrid
  where
    caveAutomataStep :: TileChunk -> TileChunk
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
