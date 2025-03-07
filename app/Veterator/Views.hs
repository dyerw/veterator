{-# LANGUAGE NamedFieldPuns #-}

module Veterator.Views where

import Data.Extra.List (chunk)
import Display.View (View (..), empty, gridLayout, sparseGridLayout)
import GameState (GameState (..))
import Math.Geometry.Grid (FiniteGrid (size))
import Math.Geometry.GridMap (GridMap (..))
import Resources (ImageKey (..))
import Veterator.Model.Creature (Creature (..), CreatureType (..))
import Veterator.Model.Dungeon (Tile (..))

rootView :: GameState -> View
rootView state =
  let dungeon = stateDungeonGrid state
      (tileWidth, _) = size dungeon
      creatures = toList (stateCreatureGrid state)
      creatureViews = (\(p, (_, c)) -> (p, creatureView c)) <$> creatures
      tileViewArrays = chunk tileWidth (tileView <$> elems dungeon)
   in Group
        [ gridLayout 16 16 tileViewArrays,
          sparseGridLayout 16 16 creatureViews
        ]

creatureView :: Creature -> View
creatureView Creature {creatureType} = case creatureType of
  Adventurer -> Sprite PlayerImage
  Goblin -> Sprite GoblinImage

tileView :: Tile -> View
tileView tile =
  case tile of
    Floor -> Sprite FloorTileImage
    Wall -> Sprite WallTileImage
    _ -> empty