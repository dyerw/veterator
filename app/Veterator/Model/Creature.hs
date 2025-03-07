module Veterator.Model.Creature where

data CreatureType = Adventurer | Goblin

data Item = Gold Int | Rock

data Creature = Creature
  { creatureType :: CreatureType,
    creatureHealth :: Int,
    creatureStats :: CreatureStats,
    creatureInventory :: [Item]
  }

isHostile :: Creature -> Bool
isHostile Creature {creatureType = Goblin} = True
isHostile _ = False

data CreatureStats = CreatureStats {maxHealth :: Int, minDamage :: Int, maxDamage :: Int}