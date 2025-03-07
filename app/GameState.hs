{-# LANGUAGE NamedFieldPuns #-}

-- | GameState is the pure core of the game that implements all game rules
-- and logic, ignoring presentational elements like sound and animations
module GameState where

import Data.Extra.Maybe (replace)
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.UUID (UUID)
import Gen.Dungeon
  ( DungeonGeneration (DungeonGeneration, generationCreatures, generationPlayerUUID, generationTiles),
  )
import Linear.Affine (Affine ((.+^)))
import Linear.V2 (V2 (V2))
import System.Random (StdGen)
import Veterator.Model.Dungeon
  ( CreatureGrid,
    DungeonGrid,
    DungeonPosition,
    fromPoint,
    getCreatureAt,
    getCreaturePosition,
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

data Command = Move Dir | Attack UUID

data MoveResult = Vacant | Blocked | OutOfBounds | Enemy UUID

getMoveResult :: GameState -> DungeonPosition -> MoveResult
getMoveResult state moveTo
  | not (inBounds dungeonGrid moveTo) = OutOfBounds
  | isJust creatureAtPos = Enemy (creatureAtPos & fromJust & fst)
  | not (isEmpty dungeonGrid moveTo) = Blocked
  | otherwise = Vacant
  where
    dungeonGrid = stateDungeonGrid state
    creatureGrid = stateCreatureGrid state
    creatureAtPos = getCreatureAt moveTo creatureGrid

applyCommand :: Command -> GameState -> GameState
applyCommand (Move d) state =
  let moveTo = move d (playerPos state)
      moveResult = getMoveResult state moveTo
   in case moveResult of
        Vacant ->
          state
            { stateCreatureGrid = replace (moveCreature (statePlayerUUID state) moveTo) (stateCreatureGrid state)
            }
        Enemy id -> applyCommand (Attack id) state
        _ -> state
applyCommand (Attack enemyId) state = undefined

initialGameState :: StdGen -> DungeonGeneration -> GameState
initialGameState rng' DungeonGeneration {generationTiles, generationCreatures, generationPlayerUUID} =
  GameState
    { stateDungeonGrid = generationTiles,
      stateCreatureGrid = generationCreatures,
      statePlayerUUID = generationPlayerUUID,
      -- items = [],
      rng = rng'
    }