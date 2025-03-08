module Veterator.Model.Creature where

import Data.UUID (UUID)

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

isHostile :: Creature -> Bool
isHostile Creature {creatureType = Goblin} = True
isHostile _ = False

dealDamage :: Int -> Creature -> Creature
dealDamage i c = c {creatureHealth = creatureHealth c - i}

data CreatureStats = CreatureStats {statsMaxHealth :: Int, statsDamageRange :: (Int, Int)} deriving (Show)