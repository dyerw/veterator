{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}

module Game where

import Control.Arrow (Arrow (arr), returnA, (>>>))
import Control.Monad.Random (evalRand)
import Display.View (AbsoluteView, Camera (Camera), View (..), layout)
import FRP.Yampa (Event (..), SF, edge, mergeEvents, sscan, tagWith)
import GameState (GameM, GameState (..), getPlayerPosition, initialGameState, runGameM, tick)
import Gen.Dungeon (generateDungeon)
import Input (Controller (..))
import Linear (V2)
import System.Random (StdGen)
import Veterator.Direction (east, north, south, west)
import Veterator.Events (GameEvent (..))
import Veterator.Model.Creature (CreatureAction (..))
import Veterator.Views (rootView)

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
  absoluteView <- views -< (gameEvents, state, gameInputWindowSize gi)
  returnA -< GameOutput absoluteView

views :: SF ([GameEvent d], GameState, WindowSize) AbsoluteView
views = proc (gameEvents, state, windowSize) -> do
  camera <- followsPlayer -< (state, windowSize)
  rv <- rootView -< (state, gameEvents)
  absoluteView <- layouts -< (rv, camera)
  returnA -< absoluteView

updatesState :: StdGen -> SF (Event CreatureAction) (GameState, [GameEvent d])
updatesState seed = sscan loop (initialGameState dungeonGen, seed, []) >>> arr (\(state, _, gameEvents) -> (state, gameEvents))
  where
    -- Internally we need to hold the StdGen state
    loop (state, rng, _) event' =
      let ((nextState, gameEvents), nextRng) = runGameM (updateState state event') rng
       in (nextState, nextRng, gameEvents)
    dungeonGen = evalRand generateDungeon seed

updateState :: GameState -> Event CreatureAction -> GameM d GameState
updateState state event' = case event' of
  -- I suppose this is what makes this turn-based
  NoEvent -> pure state
  Event command -> tick command state

toAction :: (Controller -> Bool) -> CreatureAction -> SF Controller (Event CreatureAction)
toAction f command = arr f >>> edge >>> arr (tagWith command)

commands :: SF Controller (Event CreatureAction)
commands = proc controller -> do
  moveW <- (toAction controllerLeft (Move west)) -< controller
  moveE <- (toAction controllerRight (Move east)) -< controller
  moveN <- (toAction controllerUp (Move north)) -< controller
  moveS <- (toAction controllerDown (Move south)) -< controller
  returnA -< mergeEvents [moveW, moveE, moveN, moveS]

layouts :: SF (View, Camera) AbsoluteView
layouts = arr (uncurry layout)

followsPlayer :: SF (GameState, WindowSize) Camera
followsPlayer =
  arr
    ( \(state, windowSize) ->
        -- FIXME: Get a handle on coordinate transformations
        let playerTilePos = getPlayerPosition state
            playerPxPos = (* (-16)) <$> playerTilePos
            halfWindowOffset = ((`div` 2) <$> windowSize)
         in Camera (playerPxPos + halfWindowOffset) 1 windowSize
    )
