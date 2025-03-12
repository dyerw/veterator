module Veterator.AI where

import Data.UUID (UUID)
import Veterator.Model.Creature (CreatureAction)
import Veterator.Model.Dungeon (Dungeon)

type AI = Dungeon -> UUID -> CreatureAction

-- attackOnSightAI :: AI
-- attackOnSightAI dungeon actor =
--     let closestHostile =