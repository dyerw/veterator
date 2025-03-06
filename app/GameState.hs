{-# LANGUAGE NamedFieldPuns #-}

-- | GameState is the pure core of the game that implements all game rules
-- and logic, ignoring presentational elements like sound and animations
module GameState where

import Data.Extra.Maybe (replace)
import Data.Maybe (fromMaybe)
import Data.UUID (UUID)
import Dungeon
  ( CreatureGrid,
    DungeonGrid,
    DungeonPosition,
    fromPoint,
    getCreaturePosition,
    inBounds,
    isEmpty,
    moveCreature,
    toPoint,
  )
import Gen.Dungeon
  ( DungeonGeneration (DungeonGeneration, generationCreatures, generationPlayerUUID, generationTiles),
  )
import Linear.Affine (Affine ((.+^)))
import Linear.V2 (V2 (V2))
import System.Random (StdGen)

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
  { stateDungeonGrid :: DungeonGrid,
    stateCreatureGrid :: CreatureGrid,
    statePlayerUUID :: UUID,
    -- items :: [(DungeonPosition, Item)],
    rng :: StdGen
  }

playerPos :: GameState -> DungeonPosition
playerPos state =
  -- Crash if player doesn't exist in creature grid
  fromMaybe (error "FATAL: Player UUID does not exist in creature grid") $
    getCreaturePosition
      (stateCreatureGrid state)
      (statePlayerUUID state)

data Command = Move Dir

data MoveResult = Vacant | Blocked | OutOfBounds | Enemy

applyCommand :: Command -> GameState -> GameState
applyCommand (Move d) state =
  let moveTo = move d (playerPos state)
      dungeonGrid = stateDungeonGrid state
   in if isEmpty dungeonGrid moveTo && inBounds dungeonGrid moveTo
        then
          state
            { stateCreatureGrid = replace (moveCreature (statePlayerUUID state) moveTo) (stateCreatureGrid state)
            }
        else state

initialGameState :: StdGen -> DungeonGeneration -> GameState
initialGameState rng' DungeonGeneration {generationTiles, generationCreatures, generationPlayerUUID} =
  GameState
    { stateDungeonGrid = generationTiles,
      stateCreatureGrid = generationCreatures,
      statePlayerUUID = generationPlayerUUID,
      -- items = [],
      rng = rng'
    }