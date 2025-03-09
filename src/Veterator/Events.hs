module Veterator.Events where

import Data.UUID (UUID)
import Veterator.Model.Creature (Creature)

data GameEvent
  = CreatureTookDamage UUID Int
  | PlayerGainedXP Int
  | CreatureDied Creature
  deriving (Show)
