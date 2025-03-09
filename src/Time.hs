module Time where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified SDL

newtype TimeRef = TimeRef {unTimeRef :: IORef Int}

initializeTimeRef :: IO TimeRef
initializeTimeRef = do
  timeRef <- TimeRef <$> newIORef (0 :: Int)
  -- Copied this from haskanoid and don't feel like bothering to figure out wtf
  _ <- senseTimeRef timeRef
  _ <- senseTimeRef timeRef
  _ <- senseTimeRef timeRef
  _ <- senseTimeRef timeRef
  return timeRef

senseTimeRef :: TimeRef -> IO Int
senseTimeRef timeRef = do
  -- Get time passed since SDL init
  newTime <- fmap fromIntegral SDL.ticks
  -- Obtain time difference
  updateTime timeRef newTime

-- | Updates the time in an IO Ref and returns the time difference
updateTime :: TimeRef -> Int -> IO Int
updateTime timeRef newTime = do
  previousTime <- readIORef (unTimeRef timeRef)
  writeIORef (unTimeRef timeRef) newTime
  return (newTime - previousTime)

millisecsToSecs :: Int -> Double
millisecsToSecs m = fromIntegral m / 1000