module Input where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import qualified FRP.Yampa as Yampa
import Linear.Affine (Point (P))
import Linear.V2 (V2 (V2))
import SDL
  ( Event (..),
    KeyboardEventData (..),
    Keysym (..),
    MouseButtonEventData (..),
    MouseMotionEventData (..),
  )
import qualified SDL
import qualified Time

data Controller = Controller
  { controllerPos :: Point V2 Int32,
    controllerClick :: Bool,
    controllerLeft :: Bool,
    controllerUp :: Bool,
    controllerRight :: Bool,
    controllerDown :: Bool
  }
  deriving (Show)

newtype ControllerRef
  = ControllerRef (IORef Controller, Controller -> IO Controller)

initialize :: IO ControllerRef
initialize = do
  nr <- newIORef $ Controller (P (V2 0 0)) False False False False False
  return $ ControllerRef (nr, sdlGetController)

senseInput :: ControllerRef -> IO Controller
senseInput (ControllerRef (cref, sensor)) = do
  cinfo <- readIORef cref
  cinfo' <- sensor cinfo
  writeIORef cref cinfo'
  return cinfo'

senseInputWithDeltaTime :: Time.TimeRef -> ControllerRef -> IO (Yampa.DTime, Maybe Controller)
senseInputWithDeltaTime timeRef controllerRef = do
  dtSecs <- Time.millisecsToSecs <$> Time.senseTimeRef timeRef
  mInput <- Input.senseInput controllerRef
  return (dtSecs, Just mInput)

sdlGetController :: Controller -> IO Controller
sdlGetController lastController = do
  event <- SDL.pollEvent
  case event of
    Just e -> sdlGetController (handleEvent lastController e)
    Nothing -> pure lastController

handleEvent :: Controller -> SDL.Event -> Controller
handleEvent c e =
  case e of
    SDL.Event
      { eventPayload =
          SDL.MouseMotionEvent
            ( SDL.MouseMotionEventData
                { mouseMotionEventPos = pos
                }
              )
      } ->
        c {controllerPos = pos}
    SDL.Event
      { eventPayload =
          SDL.MouseButtonEvent
            ( SDL.MouseButtonEventData
                { mouseButtonEventMotion = SDL.Pressed
                }
              )
      } ->
        c {controllerClick = True}
    SDL.Event
      { eventPayload =
          SDL.MouseButtonEvent
            ( SDL.MouseButtonEventData
                { mouseButtonEventMotion = SDL.Released
                }
              )
      } ->
        c {controllerClick = False}
    SDL.Event
      { eventPayload =
          SDL.KeyboardEvent
            ( SDL.KeyboardEventData
                { keyboardEventKeyMotion = SDL.Pressed,
                  keyboardEventKeysym = SDL.Keysym {keysymKeycode = SDL.KeycodeUp}
                }
              )
      } ->
        c {controllerUp = True}
    SDL.Event
      { eventPayload =
          SDL.KeyboardEvent
            ( SDL.KeyboardEventData
                { keyboardEventKeyMotion = SDL.Released,
                  keyboardEventKeysym = SDL.Keysym {keysymKeycode = SDL.KeycodeUp}
                }
              )
      } ->
        c {controllerUp = False}
    SDL.Event
      { eventPayload =
          SDL.KeyboardEvent
            ( SDL.KeyboardEventData
                { keyboardEventKeyMotion = SDL.Pressed,
                  keyboardEventKeysym = SDL.Keysym {keysymKeycode = SDL.KeycodeRight}
                }
              )
      } ->
        c {controllerRight = True}
    SDL.Event
      { eventPayload =
          SDL.KeyboardEvent
            ( SDL.KeyboardEventData
                { keyboardEventKeyMotion = SDL.Released,
                  keyboardEventKeysym = SDL.Keysym {keysymKeycode = SDL.KeycodeRight}
                }
              )
      } ->
        c {controllerRight = False}
    SDL.Event
      { eventPayload =
          SDL.KeyboardEvent
            ( SDL.KeyboardEventData
                { keyboardEventKeyMotion = SDL.Pressed,
                  keyboardEventKeysym = SDL.Keysym {keysymKeycode = SDL.KeycodeDown}
                }
              )
      } ->
        c {controllerDown = True}
    SDL.Event
      { eventPayload =
          SDL.KeyboardEvent
            ( SDL.KeyboardEventData
                { keyboardEventKeyMotion = SDL.Released,
                  keyboardEventKeysym = SDL.Keysym {keysymKeycode = SDL.KeycodeDown}
                }
              )
      } ->
        c {controllerDown = False}
    SDL.Event
      { eventPayload =
          SDL.KeyboardEvent
            ( SDL.KeyboardEventData
                { keyboardEventKeyMotion = SDL.Pressed,
                  keyboardEventKeysym = SDL.Keysym {keysymKeycode = SDL.KeycodeLeft}
                }
              )
      } ->
        c {controllerLeft = True}
    SDL.Event
      { eventPayload =
          SDL.KeyboardEvent
            ( SDL.KeyboardEventData
                { keyboardEventKeyMotion = SDL.Released,
                  keyboardEventKeysym = SDL.Keysym {keysymKeycode = SDL.KeycodeLeft}
                }
              )
      } ->
        c {controllerLeft = False}
    _ -> c