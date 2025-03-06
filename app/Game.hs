{-# LANGUAGE Arrows #-}

module Game where

import Control.Arrow (Arrow (arr), returnA, (>>>))
import Control.Monad.Random (evalRand)
import Display.View (View)
import FRP.Yampa (Event (..), SF, edge, mergeEvents, sscan, tagWith)
import GameState (Command (Move), Dir (..), GameState (..), applyCommand, initialGameState)
import Gen.Dungeon (generateDungeon)
import Input (Controller (..))
import System.Random (StdGen)
import Veterator.Views (rootView)

entireGame :: StdGen -> SF Controller View
entireGame seed = commands >>> updatesState seed >>> views

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
