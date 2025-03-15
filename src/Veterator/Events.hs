module Veterator.Events where

import Data.UUID (UUID)
import Veterator.Model.Creature (Creature)

data GameEvent d
  = CreatureTookDamage UUID Int
  | CreatureAttacked UUID d
  | PlayerGainedXP Int
  | CreatureDied Creature
  deriving (Show)
