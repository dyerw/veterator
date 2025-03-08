{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Game where

import Control.Arrow (Arrow (arr), returnA, (>>>))
import Control.Monad.Random (evalRand)
import Data.IdentityList (IdentityList)
import qualified Data.IdentityList as IL
import Data.Text (pack)
import Display.View (AbsoluteView, Camera (Camera), GameView, UIView, View (..), layoutScreen, px)
import FRP.Yampa (Event (..), SF, after, dpSwitch, edge, isEvent, mergeEvents, notYet, sscan, tagWith)
import GameState (Command (Move), Dir (..), GameM, GameState (..), applyCommand, getPlayerPosition, initialGameState, runGameM)
import Gen.Dungeon (generateDungeon)
import Input (Controller (..))
import Linear (V2 (V2))
import System.Random (StdGen)
import Veterator.Events (GameEvent (..))
import Veterator.Model.Dungeon (getCreaturePosition)
import Veterator.Views (uiView, worldView)

type WindowSize = V2 Int

data GameInput = GameInput
  { gameInputController :: Controller,
    gameInputWindowSize :: WindowSize
  }

newtype GameOutput = GameOutput
  { gameOutputAbsoluteView :: AbsoluteView
  }

entireGame :: StdGen -> SF GameInput GameOutput
entireGame seed = proc gi -> do
  command <- commands -< (gameInputController gi)
  (state, gameEvents) <- (updatesState seed) -< command
  camera <- followsPlayer -< (state, gameInputWindowSize gi)
  wv <- viewsWorld -< state
  effects <- damageEffects -< (state, gameEvents)
  uiv <- viewsUI -< state
  absoluteView <- layoutsScreen -< (Group [wv, effects], uiv, camera)
  returnA -< GameOutput absoluteView

updatesState :: StdGen -> SF (Event Command) (GameState, [GameEvent])
updatesState seed = sscan loop (initialGameState dungeonGen, seed, []) >>> arr (\(state, _, gameEvents) -> (state, gameEvents))
  where
    -- Internally we need to hold the StdGen state
    loop (state, rng, _) event' =
      let ((nextState, gameEvents), nextRng) = runGameM (updateState state event') rng
       in (nextState, nextRng, gameEvents)
    dungeonGen = evalRand (generateDungeon 100 100) seed

updateState :: GameState -> Event Command -> GameM GameState
updateState state event' = case event' of
  NoEvent -> pure state
  Event command -> applyCommand command state

toCommand :: (Controller -> Bool) -> Command -> SF Controller (Event Command)
toCommand f command = arr f >>> edge >>> arr (tagWith command)

commands :: SF Controller (Event Command)
commands = proc controller -> do
  moveW <- (toCommand controllerLeft (Move W)) -< controller
  moveE <- (toCommand controllerRight (Move E)) -< controller
  moveN <- (toCommand controllerUp (Move N)) -< controller
  moveS <- (toCommand controllerDown (Move S)) -< controller
  returnA -< mergeEvents [moveW, moveE, moveN, moveS]

viewsWorld :: SF GameState View
viewsWorld = arr worldView

viewsUI :: SF GameState View
viewsUI = arr uiView

layoutsScreen :: SF (GameView, UIView, Camera) AbsoluteView
layoutsScreen = arr (\(gv, uiv, c) -> layoutScreen gv uiv c)

followsPlayer :: SF (GameState, WindowSize) Camera
followsPlayer =
  arr
    ( \(state, windowSize) ->
        -- FIXME: Get a handle on coordinate transformations
        let playerTilePos = getPlayerPosition state
            playerPxPos = (* (-16)) <$> uncurry px playerTilePos
            halfWindowOffset = ((`div` 2) <$> windowSize)
         in Camera (playerPxPos + halfWindowOffset) 1
    )

-- | Paired with an event that indicates to destroy
data Die = Die deriving (Show)

type Destructable a = (a, Event Die)

isDestroyed :: Destructable a -> Bool
isDestroyed = isEvent . snd

type EffectSF = SF () (Destructable View)

damageView :: GameState -> GameEvent -> View
damageView state (CreatureTookDamage uuid amount) =
  case getCreaturePosition (stateDungeon state) uuid of
    Just (x, y) ->
      Translate (V2 (16 * x) (16 * y)) (Label (pack $ show amount))
    Nothing -> Group []

damageEffect :: GameState -> GameEvent -> EffectSF
damageEffect s e = proc () -> do
  destroyEvent <- after 3 Die -< ()
  returnA -< (damageView s e, destroyEvent)

-- Dynamically manages a collection of View signals that can destroy themselves via dpSwitch
damageEffects :: SF (GameState, [GameEvent]) View
damageEffects = damageEffects' IL.empty >>> arr (Group . fmap fst . IL.elems)
  where
    damageEffects' :: IdentityList EffectSF -> SF (GameState, [GameEvent]) (IdentityList (Destructable View))
    damageEffects' effs =
      dpSwitch
        router
        effs
        (arr killAndSpawn >>> notYet)
        (\sfs f -> damageEffects' (f sfs))

    router :: forall sf. (GameState, [GameEvent]) -> IdentityList sf -> IdentityList ((), sf)
    router _ = fmap ((),)

    killAndSpawn :: ((GameState, [GameEvent]), IdentityList (Destructable View)) -> Event (IdentityList EffectSF -> IdentityList EffectSF)
    killAndSpawn ((state, gameEvents), views) =
      let additions = IL.insert . damageEffect state <$> gameEvents
          deletions = IL.delete <$> IL.keys (IL.filter isDestroyed views)
       in Event $ foldl (.) id (additions <> deletions)
