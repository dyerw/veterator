-- Cardinal and ordinal directions for 2d integer vectors
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}

module Veterator.Direction where

import Linear (V2 (..), perp)
import Linear.Vector ((^+^))

class Direction d where
  toVec :: d -> V2 Int
  move :: d -> V2 Int -> V2 Int
  move d v = v ^+^ toVec d
  {-# MINIMAL toVec #-}

data Cardinal = N | E | S | W deriving (Show, Eq)

instance Direction Cardinal where
  toVec :: Cardinal -> V2 Int
  toVec N = V2 0 (-1)
  toVec E = perp $ toVec N
  toVec S = perp $ toVec E
  toVec W = perp $ toVec S

data Ordinal = NW | NE | SW | SE deriving (Show, Eq)

instance Direction Ordinal where
  toVec :: Ordinal -> V2 Int
  toVec NW = move N $ move W (V2 0 0)
  toVec NE = move N $ move E (V2 0 0)
  toVec SW = move S $ move W (V2 0 0)
  toVec SE = move S $ move E (V2 0 0)

data Compass = Ordinal Ordinal | Cardinal Cardinal

instance Direction Compass where
  toVec :: Compass -> V2 Int
  toVec (Ordinal o) = toVec o
  toVec (Cardinal c) = toVec c

north :: Compass
north = Cardinal N

pattern North :: Compass
pattern North = Cardinal N

west :: Compass
west = Cardinal W

pattern West :: Compass
pattern West = Cardinal W

east :: Compass
east = Cardinal E

pattern East :: Compass
pattern East = Cardinal E

south :: Compass
south = Cardinal S

pattern South :: Compass
pattern South = Cardinal S

northeast :: Compass
northeast = Ordinal NE

pattern Northeast :: Compass
pattern Northeast = Ordinal NE

northwest :: Compass
northwest = Ordinal NW

southeast :: Compass
southeast = Ordinal SE

southwest :: Compass
southwest = Ordinal SW

allCardinals :: [Cardinal]
allCardinals = [N, E, S, W]

allOrdinals :: [Ordinal]
allOrdinals = [NE, SE, SW, NW]

allCompass :: [Compass]
allCompass = (Cardinal <$> allCardinals) <> (Ordinal <$> allOrdinals)
