module Veterator.Events where

import Data.UUID (UUID)

data GameEvent = CreatureTookDamage UUID Int deriving (Show)
