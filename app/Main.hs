{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Display
import qualified FRP.Yampa as Yampa
import Game
  ( GameInput (GameInput),
    GameOutput (GameOutput, gameOutputCamera, gameOutputView),
    entireGame,
  )
import qualified Input
import Linear (V2)
import qualified Resources
import qualified SDL
import qualified SDL.Font as Font
import System.Random (initStdGen)
import qualified Time

main :: IO ()
main = do
  Font.initialize
  (window, renderer) <- Display.initialize
  resources <- Resources.loadResources renderer

  controllerRef <- Input.initialize
  timeRef <- Time.initializeTimeRef
  stdGen <- initStdGen

  Yampa.reactimate
    ( do
        initialController <- Input.senseInput controllerRef
        initialWindowSize <- getWindowSize window
        pure (GameInput initialController initialWindowSize)
    )
    -- Input retrieval
    -- (argument indicates if we can block, but we never do)
    ( \_ -> do
        dtSecs <- Time.millisecsToSecs <$> Time.senseTimeRef timeRef
        controller <- Input.senseInput controllerRef
        windowSize <- getWindowSize window
        pure (dtSecs, Just $ GameInput controller windowSize)
    )
    -- Output processing
    ( \_ GameOutput {gameOutputCamera, gameOutputView} -> do
        Display.render resources renderer gameOutputCamera gameOutputView
        -- Always continue
        return False
    )
    -- Root Signal Function (Controller -> View)
    (entireGame stdGen)
  where
    getWindowSize :: SDL.Window -> IO (V2 Int)
    getWindowSize = fmap (fmap fromIntegral) . SDL.get . SDL.windowSize
