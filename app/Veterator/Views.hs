{-# LANGUAGE NamedFieldPuns #-}

module Veterator.Views where

import Data.Extra.List (chunk)
import Data.Extra.Tuple (mapSnd)
import Data.Text (pack)
import Display.View (Side (BottomSide), TextAlignment (Centered), View (..), empty, gridLayout, sparseGridLayout)
import GameState (GameState (..))
import Math.Geometry.Grid (FiniteGrid (size))
import Math.Geometry.GridMap (GridMap (..))
import Resources (ImageKey (..))
import Veterator.Model.Creature (Creature (..), CreatureType (..))
import Veterator.Model.Dungeon (Dungeon (..), Tile (..))

uiView :: GameState -> View
uiView state = CenterX $ From BottomSide 20 $ Label Centered (pack $ show $ statePlayerXP state)

worldView :: GameState -> View
worldView state =
  let dungeon = stateDungeon state
      tiles = dungeonTiles dungeon
      creatures = toList (dungeonCreatures dungeon)
      (tileWidth, _) = size tiles
      creatureViews = mapSnd creatureView <$> creatures
      tileViewArrays = chunk tileWidth (tileView <$> elems tiles)
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