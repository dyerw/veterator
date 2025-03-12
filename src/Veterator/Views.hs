{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Veterator.Views where

import Control.Arrow (returnA, (>>>))
import qualified Data.Bifunctor as Bi
import Data.Extra.Tuple (mapSnd)
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import Debug.Extra (observe)
import Display.View (Side (BottomSide), TextAlignment (Centered), View (..), empty, gridSF, sparseGridLayout)
import FRP (andDespawnAfterSecs, spawning)
import FRP.Yampa (SF, constant, time)
import GameState (GameState (..), getPlayerPosition)
import Linear (V2 (V2))
import Math.Geometry.GridMap (GridMap (..))
import Resources (ImageKey (..))
import Veterator.Events (GameEvent (..))
import Veterator.Model.Creature (Creature (..), CreatureType (..))
import Veterator.Model.Dungeon (Dungeon (..), Tile (..), tileSection)

rootView :: SF (GameState, [GameEvent]) View
rootView = proc (state, events) -> do
  wv <- worldView -< (state, events)
  let ui = uiView state
  let view = Group [wv, ui]

  returnA -< view

uiView :: GameState -> View
uiView state =
  CenterX $
    From BottomSide 20 $
      Label Centered (pack $ show $ statePlayerXP state)

visibleTileSectionSize :: Int
visibleTileSectionSize = 100

worldView :: SF (GameState, [GameEvent]) View
worldView = proc (state, events) -> do
  cv <- creaturesView -< (state, events)
  let tv = tilesView state
  returnA -< Group [tv, cv]

creaturesView :: SF (GameState, [GameEvent]) View
creaturesView = proc (state, events) -> do
  let dungeon = stateDungeon state
  let creatures = Bi.second (,events) <$> toList (dungeonCreatures dungeon)
  view <- gridSF 16 16 (creatureId . fst) creatureView -< creatures
  returnA -< (observe view)

tilesView :: GameState -> View
tilesView state =
  let dungeon = stateDungeon state
      tiles = dungeonTiles dungeon
      (x, y) = getPlayerPosition state
      visibleTiles =
        tileSection
          (x - div visibleTileSectionSize 2, y - div visibleTileSectionSize 2)
          visibleTileSectionSize
          visibleTileSectionSize
          tiles
      tileViewArrays = mapSnd tileView <$> visibleTiles
   in sparseGridLayout 16 16 tileViewArrays

animate :: Double -> Double -> SF View View
animate dx dy = proc view -> do
  t <- time -< ()
  let x = round (t * dx)
  let y = round (t * dy)
  returnA -< Translate (V2 x y) view

floatingDamageNumber :: Int -> SF () (Maybe View)
floatingDamageNumber amount =
  constant view
    >>> animate 20 (-20)
    >>> andDespawnAfterSecs 2
  where
    view = Label Centered (pack $ show amount)

creatureView :: SF (Creature, [GameEvent]) View
creatureView = proc (Creature {creatureId, creatureType}, events) -> do
  let damageTaken =
        mapMaybe
          ( \case
              CreatureTookDamage eid amt ->
                if eid == creatureId then Just amt else Nothing
              _ -> Nothing
          )
          events
  damageNumbers <- spawning floatingDamageNumber -< ((), damageTaken)

  let sprite = case creatureType of
        Adventurer -> Sprite PlayerImage
        Goblin -> Sprite GoblinImage
  returnA -< Group (sprite : damageNumbers)

tileView :: Tile -> View
tileView tile =
  case tile of
    Floor -> Sprite FloorTileImage
    Wall -> Sprite WallTileImage
    _ -> empty