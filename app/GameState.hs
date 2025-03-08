{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | GameState is the pure core of the game that implements all game rules
-- and logic, ignoring presentational elements like sound and animations
module GameState where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Random.Lazy (MonadRandom (..), Rand, runRandT)
import Control.Monad.Writer (MonadWriter (..), WriterT (runWriterT))
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.UUID (UUID)
import Gen.Dungeon
  ( DungeonGeneration
      ( DungeonGeneration,
        generationCreatures,
        generationItems,
        generationPlayerUUID,
        generationTiles
      ),
  )
import Linear.Affine (Affine ((.+^)))
import Linear.V2 (V2 (V2))
import System.Random (StdGen)
import Veterator.Events (GameEvent (CreatureTookDamage))
import Veterator.Model.Creature (Creature (..), CreatureStats (statsDamageRange), dealDamage)
import Veterator.Model.Dungeon
  ( Dungeon (..),
    DungeonPosition,
    fromPoint,
    getCreatureAt,
    getCreatureWithPosition,
    inBounds,
    isEmpty,
    moveCreature,
    toPoint,
    updateCreature,
  )

data Dir = N | NE | E | SE | S | SW | W | NW

move :: Dir -> DungeonPosition -> DungeonPosition
move N pos = fromPoint $ toPoint pos .+^ V2 0 (-1)
move NE pos = fromPoint $ toPoint pos .+^ V2 1 (-1)
move E pos = fromPoint $ toPoint pos .+^ V2 1 0
move SE pos = fromPoint $ toPoint pos .+^ V2 1 1
move S pos = fromPoint $ toPoint pos .+^ V2 0 1
move SW pos = fromPoint $ toPoint pos .+^ V2 (-1) 1
move W pos = fromPoint $ toPoint pos .+^ V2 (-1) 0
move NW pos = fromPoint $ toPoint pos .+^ V2 (-1) (-1)

data GameState = GameState
  { stateDungeon :: Dungeon,
    statePlayerUUID :: UUID
  }

newtype GameM a = GameM (WriterT [GameEvent] (Rand StdGen) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadRandom,
      (MonadWriter [GameEvent])
    )

runGameM :: GameM a -> (StdGen -> ((a, [GameEvent]), StdGen))
runGameM (GameM m) = runIdentity . runRandT (runWriterT m)

getPlayerWithPosition :: GameState -> (DungeonPosition, Creature)
getPlayerWithPosition state =
  fromMaybe (error "FATAL: Player UUID does not exist in creature grid") $
    getCreatureWithPosition
      (stateDungeon state)
      (statePlayerUUID state)

getPlayerPosition :: GameState -> DungeonPosition
getPlayerPosition = fst . getPlayerWithPosition

getPlayer :: GameState -> Creature
getPlayer = snd . getPlayerWithPosition

data Command = Move Dir | Attack Creature

data MoveResult = Vacant | Blocked | OutOfBounds | Enemy Creature

getMoveResult :: GameState -> DungeonPosition -> MoveResult
getMoveResult state destination
  | not (inBounds dungeon destination) = OutOfBounds
  | isJust creatureAtPos = Enemy (fromJust creatureAtPos)
  | not (isEmpty dungeon destination) = Blocked
  | otherwise = Vacant
  where
    dungeon = stateDungeon state
    creatureAtPos = getCreatureAt dungeon destination

applyCommand :: Command -> GameState -> GameM GameState
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
    _ -> pure state
  where
    destination = move d (getPlayerPosition state)
applyCommand (Attack Creature {creatureId}) state = do
  let playerStats = creatureStats (getPlayer state)
  damage <- getRandomR (statsDamageRange playerStats)
  _ <- tell [CreatureTookDamage creatureId damage]
  pure $
    state
      { stateDungeon =
          updateCreature
            (stateDungeon state)
            creatureId
            (dealDamage damage)
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
      { stateDungeon =
          Dungeon
            { dungeonTiles = generationTiles,
              dungeonCreatures = generationCreatures,
              dungeonItems = generationItems
            },
        statePlayerUUID = generationPlayerUUID
      }