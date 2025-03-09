{-# LANGUAGE NamedFieldPuns #-}

module Veterator.Model.Creature where

import Data.UUID (UUID)
import Veterator.Dir (Dir)

data CreatureType = Adventurer | Goblin deriving (Show)

data Item = Gold Int | Rock deriving (Show)

data Creature = Creature
  { creatureId :: UUID,
    creatureType :: CreatureType,
    creatureHealth :: Int,
    creatureStats :: CreatureStats,
    creatureInventory :: [Item]
  }
  deriving (Show)

isHostile :: Creature -> Creature -> Bool
isHostile Creature {creatureType = Goblin} Creature {creatureType = Adventurer} = True
isHostile Creature {creatureType = Adventurer} Creature {creatureType = Goblin} = True
isHostile _ _ = False

isAlive :: Creature -> Bool
isAlive Creature {creatureHealth} = creatureHealth > 0

dealDamage :: Int -> Creature -> Creature
dealDamage i c = c {creatureHealth = creatureHealth c - i}

data CreatureStats = CreatureStats {statsMaxHealth :: Int, statsDamageRange :: (Int, Int)} deriving (Show)

data CreatureAction = Move Dir | Attack Creature