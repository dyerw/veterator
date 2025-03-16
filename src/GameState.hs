{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | GameState is the pure core of the game that implements all game rules
-- and logic, ignoring presentational elements like sound and animations
-- it emits events that can be used drive sound/animations
module GameState where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Random.Lazy (MonadRandom (..), MonadTrans (lift), Rand, runRandT)
import Control.Monad.Writer (MonadWriter (..), WriterT (runWriterT))
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Set as Set
import Data.UUID (UUID)
import Gen.Dungeon
  ( DungeonGeneration
      ( DungeonGeneration,
        generationCreatures,
        generationItems,
        generationPlayerUUID,
        generationTiles
      ),
    fillNeighboringChunks,
  )
import Linear (V2)
import System.Random (StdGen)
import Veterator.Direction (move)
import Veterator.Events (GameEvent (CreatureDied, CreatureTookDamage, PlayerGainedXP))
import Veterator.Model.Creature (Creature (..), CreatureAction (..), CreatureStats (statsDamageRange), dealDamage, isAlive)
import Veterator.Model.Dungeon
  ( ChunkPosition,
    Dungeon (..),
    DungeonPosition,
    dungeonPosToTileIndex,
    getCreature,
    getCreatureAt,
    getCreatureWithPosition,
    isEmpty,
    moveCreature,
    removeDeadCreatures,
    tilesVisibleToCreature,
    updateCreature,
  )

data GameState = GameState
  { stateDungeon :: Dungeon,
    stateVisibleTiles :: Set.Set (V2 Int),
    stateExploredTiles :: Set.Set (V2 Int),
    statePlayerUUID :: UUID,
    statePlayerXP :: Int
  }

-- Maybe we make this StateT GameState too?
newtype GameM d a = GameM (WriterT [GameEvent d] (Rand StdGen) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadRandom,
      (MonadWriter [GameEvent d])
    )

logEvent :: GameEvent d -> GameM d ()
logEvent = tell . pure

logEvents :: [GameEvent d] -> GameM d ()
logEvents = tell

runGameM :: GameM d a -> (StdGen -> ((a, [GameEvent d]), StdGen))
runGameM (GameM m) = runIdentity . runRandT (runWriterT m)

getPlayerWithPosition :: GameState -> (DungeonPosition, Creature)
getPlayerWithPosition state =
  fromMaybe (error "FATAL: Player UUID does not exist in creature grid") $
    getCreatureWithPosition
      (stateDungeon state)
      (statePlayerUUID state)

getPlayerPosition :: GameState -> DungeonPosition
getPlayerPosition = fst . getPlayerWithPosition

getPlayerChunk :: GameState -> ChunkPosition
getPlayerChunk = fst . dungeonPosToTileIndex . getPlayerPosition

getPlayer :: GameState -> Creature
getPlayer = snd . getPlayerWithPosition

data MoveResult = Vacant | Blocked | Enemy Creature

getMoveResult :: GameState -> DungeonPosition -> MoveResult
getMoveResult state destination
  | isJust creatureAtPos = Enemy (fromJust creatureAtPos)
  | not (isEmpty dungeon destination) = Blocked
  | otherwise = Vacant
  where
    dungeon = stateDungeon state
    creatureAtPos = getCreatureAt dungeon destination

tick :: CreatureAction -> GameState -> GameM d GameState
tick command state = applyCommand command state >>= cleanup

applyCommand :: CreatureAction -> GameState -> GameM d GameState
applyCommand (Move d) state =
  case getMoveResult state destination of
    Vacant ->
      pure $
        state
          { stateDungeon =
              moveCreature
                (stateDungeon state)
                (statePlayerUUID state)
                destination
          }
    Enemy enemyId -> applyCommand (Attack enemyId) state
    -- bonk
    _ -> pure state
  where
    destination = move d (getPlayerPosition state)
applyCommand (Attack Creature {creatureId}) state = do
  let playerStats = creatureStats (getPlayer state)
  damage <- getRandomR (statsDamageRange playerStats)
  nextState <- damageCreature creatureId damage state
  let creatureDied = maybe False (not . isAlive) (getCreature (stateDungeon nextState) creatureId)
  if creatureDied
    then
      gainXP 10 nextState
    else
      pure nextState

cleanup :: GameState -> GameM d GameState
cleanup state = do
  -- Cleanup dead creatures
  let dungeon = stateDungeon state
  let (deadCreatures, nextDungeon) = removeDeadCreatures (stateDungeon state)
  _ <- logEvents $ CreatureDied <$> deadCreatures

  -- Generate new chunks on demand
  let tiles = dungeonTiles dungeon
  let chunkPos = getPlayerChunk state
  nextTiles <- GameM $ lift $ fillNeighboringChunks tiles chunkPos
  let nextDungeon' = nextDungeon {dungeonTiles = nextTiles}

  -- Update FOV and explored
  let nextVisibleTiles = Set.fromList $ tilesVisibleToCreature nextDungeon' (statePlayerUUID state) 10
  let nextExploredTiles = Set.union nextVisibleTiles (stateExploredTiles state)

  pure $
    state
      { stateDungeon = nextDungeon',
        stateVisibleTiles = nextVisibleTiles,
        stateExploredTiles = nextExploredTiles
      }

gainXP :: Int -> GameState -> GameM d GameState
gainXP amount state = do
  _ <- logEvent $ PlayerGainedXP amount
  pure $ state {statePlayerXP = statePlayerXP state + amount}

damageCreature :: UUID -> Int -> GameState -> GameM d GameState
damageCreature creatureId amount state = do
  _ <- logEvent $ CreatureTookDamage creatureId amount
  pure $
    state
      { stateDungeon =
          updateCreature
            (stateDungeon state)
            creatureId
            (dealDamage amount)
      }

initialGameState :: DungeonGeneration -> GameState
initialGameState
  DungeonGeneration
    { generationTiles,
      generationCreatures,
      generationPlayerUUID,
      generationItems
    } =
    GameState
      { stateDungeon = dungeon,
        statePlayerUUID = generationPlayerUUID,
        statePlayerXP = 0,
        stateVisibleTiles = visibleTiles,
        stateExploredTiles = visibleTiles
      }
    where
      dungeon =
        Dungeon
          { dungeonTiles = generationTiles,
            dungeonCreatures = generationCreatures,
            dungeonItems = generationItems
          }
      visibleTiles = Set.fromList $ tilesVisibleToCreature dungeon generationPlayerUUID 10