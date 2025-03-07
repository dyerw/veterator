{-# LANGUAGE NamedFieldPuns #-}

-- | GameState is the pure core of the game that implements all game rules
-- and logic, ignoring presentational elements like sound and animations
module GameState where

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
import System.Random (StdGen, uniformR)
import Veterator.Model.Creature (Creature (..), CreatureStats (statsDamageRange))
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
    statePlayerUUID :: UUID,
    -- items :: [(DungeonPosition, Item)],
    rng :: StdGen
  }

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

applyCommand :: Command -> GameState -> GameState
applyCommand (Move d) state =
  case getMoveResult state destination of
    Vacant ->
      state
        { stateDungeon = moveCreature (stateDungeon state) (statePlayerUUID state) destination
        }
    Enemy enemyId -> applyCommand (Attack enemyId) state
    _ -> state
  where
    destination = move d (getPlayerPosition state)
applyCommand (Attack enemyCreature) state = undefined
  where
    playerStats = creatureStats (getPlayer state)
    (damage, nextRng) = uniformR (statsDamageRange playerStats) (rng state)

initialGameState :: StdGen -> DungeonGeneration -> GameState
initialGameState
  rng'
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
        statePlayerUUID = generationPlayerUUID,
        rng = rng'
      }