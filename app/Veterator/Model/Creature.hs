module Veterator.Model.Creature where

import Data.UUID (UUID)

data CreatureType = Adventurer | Goblin

data Item = Gold Int | Rock

data Creature = Creature
  { creatureId :: UUID,
    creatureType :: CreatureType,
    creatureHealth :: Int,
    creatureStats :: CreatureStats,
    creatureInventory :: [Item]
  }

isHostile :: Creature -> Bool
isHostile Creature {creatureType = Goblin} = True
isHostile _ = False

data CreatureStats = CreatureStats {statsMaxHealth :: Int, statsDamageRange :: (Int, Int)}