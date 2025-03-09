{-# LANGUAGE NamedFieldPuns #-}

module Veterator.Views where

import Data.Extra.Tuple (mapSnd)
import Data.Text (pack)
import Display.View (Side (BottomSide), TextAlignment (Centered), View (..), empty, sparseGridLayout)
import GameState (GameState (..), getPlayerPosition)
import Math.Geometry.GridMap (GridMap (..))
import Resources (ImageKey (..))
import Veterator.Model.Creature (Creature (..), CreatureType (..))
import Veterator.Model.Dungeon (Dungeon (..), Tile (..), tileSection)

uiView :: GameState -> View
uiView state = CenterX $ From BottomSide 20 $ Label Centered (pack $ show $ statePlayerXP state)

visibleTileSectionSize :: Int
visibleTileSectionSize = 100

worldView :: GameState -> View
worldView state =
  let dungeon = stateDungeon state
      tiles = dungeonTiles dungeon
      creatures = toList (dungeonCreatures dungeon)
      (x, y) = getPlayerPosition state
      visibleTiles =
        tileSection
          (x - div visibleTileSectionSize 2, y - div visibleTileSectionSize 2)
          visibleTileSectionSize
          visibleTileSectionSize
          tiles
      creatureViews = mapSnd creatureView <$> creatures
      tileViewArrays = mapSnd tileView <$> visibleTiles
   in Group
        [ sparseGridLayout 16 16 tileViewArrays,
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