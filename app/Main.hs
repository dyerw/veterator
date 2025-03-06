module Main where

import qualified Display
import qualified FRP.Yampa as Yampa
import Game (entireGame)
import qualified Input
import qualified Resources
import System.Random (initStdGen)
import qualified Time

main :: IO ()
main = do
  (_window, renderer) <- Display.initialize
  resources <- Resources.loadResources renderer

  controllerRef <- Input.initialize
  timeRef <- Time.initializeTimeRef
  stdGen <- initStdGen

  Yampa.reactimate
    -- Initial action
    (Input.senseInput controllerRef)
    -- Input retrieval
    -- (argument indicates if we can block, but we never do)
    (\_ -> Input.senseInputWithDeltaTime timeRef controllerRef)
    -- Output processing
    ( \_ view -> do
        Display.render resources renderer view
        -- Always continue
        return False
    )
    -- Root Signal Function (Controller -> View)
    (entireGame stdGen)
