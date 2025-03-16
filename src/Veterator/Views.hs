{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Veterator.Views where

import Control.Arrow (Arrow (arr), returnA, (>>>))
import qualified Data.Bifunctor as Bi
import Data.Extra.Tuple (mapSnd, toSnd)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (pack)
import Display.View
  ( Color,
    Side (BottomSide),
    SpriteSheet (SpriteSheet),
    TextAlignment (Centered),
    View (..),
    black,
    empty,
    gridSF,
    setOpacity,
    sparseGridLayout,
    stationaryGridSF,
  )
import FRP (andDespawnAfterSecs, spawning)
import FRP.Yampa (SF, Time, after, constant, edgeBy, rSwitch, switch, time)
import GameState (GameState (..), getPlayerPosition)
import Linear (V2 (V2))
import Math.Geometry.GridMap (GridMap (..))
import Resources (ImageKey (..))
import Veterator.Events (GameEvent (..))
import Veterator.Model.Creature (Creature (..), CreatureType (..))
import Veterator.Model.Dungeon (Dungeon (..), DungeonPosition, Tile (..), fromTup, tileSection)

rootView :: SF (GameState, [GameEvent d]) View
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

renderedTileSectionSize :: Int
renderedTileSectionSize = 100

renderedTiles :: GameState -> [(DungeonPosition, Tile)]
renderedTiles state =
  let (V2 playerX playerY) = getPlayerPosition state
   in tileSection
        (V2 (playerX - div renderedTileSectionSize 2) (playerY - div renderedTileSectionSize 2))
        renderedTileSectionSize
        renderedTileSectionSize
        ((dungeonTiles . stateDungeon) state)

tileSize :: Int
tileSize = 32

worldView :: SF (GameState, [GameEvent d]) View
worldView = proc (state, events) -> do
  cv <- creaturesView -< (state, events)
  rts <- arr renderedTiles -< state
  let tv = tilesView rts
  fow <- fogOfWarView -< (fst <$> rts, state)
  returnA -< Group [tv, cv, fow]

creaturesView :: SF (GameState, [GameEvent d]) View
creaturesView = proc (state, events) -> do
  let dungeon = stateDungeon state
  let creatures = Bi.bimap fromTup (,events) <$> toList (dungeonCreatures dungeon)
  view <- gridSF tileSize tileSize (creatureId . fst) creatureView -< creatures
  returnA -< view

fogOfWarView :: SF ([DungeonPosition], GameState) View
fogOfWarView = proc (tiles, state) -> do
  let exploredTiles = (stateExploredTiles state)
  let visibleTiles = (stateVisibleTiles state)
  let toFOW t =
        if Set.member t exploredTiles
          then
            if Set.member t visibleTiles then Revealed else Discovered
          else Hidden
  let fowTiles = toSnd toFOW <$> tiles
  view <- stationaryGridSF tileSize tileSize fowTileView -< fowTiles
  returnA -< view

data FOW = Revealed | Discovered | Hidden

fowTileView :: SF FOW View
fowTileView = proc fow -> do
  let opacity = case fow of
        Revealed -> 0.0
        Discovered -> 0.5
        Hidden -> 1.0
  interpolatedOpacity <- interpolating 0.3 1.0 -< opacity
  returnA -< tileRect (setOpacity black interpolatedOpacity)

tileRect :: Color -> View
tileRect = Rect (V2 tileSize tileSize)

tilesView :: [(DungeonPosition, Tile)] -> View
tilesView tiles = sparseGridLayout tileSize tileSize (mapSnd tileView <$> tiles)

-- A signal function that when the input changes interpolates
-- to the new value instead of immediately returning it
interpolating :: Time -> Double -> SF Double Double
interpolating duration initial =
  edgeBy nextInterpolation initial >>> proc next -> do
    v <- rSwitch (constant initial) -< ((), next)
    returnA -< v
  where
    nextInterpolation :: Double -> Double -> Maybe (SF () Double)
    nextInterpolation prev curr | curr /= prev = Just $ interpolateTo duration prev curr
    nextInterpolation _ _ = Nothing

interpolateTo :: Time -> Double -> Double -> SF () Double
interpolateTo duration from to =
  switch
    ( proc () -> do
        t <- time -< ()
        e <- after duration () -< ()
        let v = from + ((to - from) * (t / duration))
        returnA -< (v, e)
    )
    $ const (constant to)

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

-- attackBump :: SF (Event Dir) (View -> View)
-- attackBump = proc

creatureView :: SF (Creature, [GameEvent d]) View
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
        Adventurer -> adventurerSprite
        Goblin -> goblinSprite
  returnA -< Group (sprite : damageNumbers)

goblinSprite :: View
goblinSprite = SheetSprite 7 0 (SpriteSheet 32 32 MonstersSpriteSheet)

adventurerSprite :: View
adventurerSprite = SheetSprite 0 1 (SpriteSheet 32 32 RoguesSpriteSheet)

tileView :: Tile -> View
tileView tile =
  case tile of
    Floor -> floorSprite
    Wall -> wallSprite
    _ -> empty

floorSprite :: View
floorSprite = SheetSprite 0 7 (SpriteSheet 32 32 TilesSpriteSheet)

wallSprite :: View
wallSprite = SheetSprite 0 1 (SpriteSheet 32 32 TilesSpriteSheet)