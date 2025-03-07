{-# LANGUAGE Arrows #-}

module Game where

import Control.Arrow (Arrow (arr), returnA, (>>>))
import Control.Monad.Random (evalRand)
import Display (Camera (Camera))
import Display.View (View, px)
import FRP.Yampa (Event (..), SF, edge, mergeEvents, sscan, tagWith)
import GameState (Command (Move), Dir (..), GameState (..), applyCommand, getPlayerPosition, initialGameState)
import Gen.Dungeon (generateDungeon)
import Input (Controller (..))
import Linear (V2)
import System.Random (StdGen)
import Veterator.Views (rootView)

type WindowSize = V2 Int

data GameInput = GameInput
  { gameInputController :: Controller,
    gameInputWindowSize :: WindowSize
  }

data GameOutput = GameOutput
  { gameOutputCamera :: Camera,
    gameOutputView :: View
  }

entireGame :: StdGen -> SF GameInput GameOutput
entireGame seed = proc gi -> do
  command <- commands -< (gameInputController gi)
  state <- (updatesState seed) -< command
  view <- views -< state
  camera <- followsPlayer -< (state, gameInputWindowSize gi)
  returnA -< GameOutput camera view

updatesState :: StdGen -> SF (Event Command) GameState
updatesState seed = sscan updateState (initialGameState seed dungeonGen)
  where
    dungeonGen = evalRand (generateDungeon 100 100) seed

updateState :: GameState -> Event Command -> GameState
updateState state event = case event of
  NoEvent -> state
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

views :: SF GameState View
views = arr rootView

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