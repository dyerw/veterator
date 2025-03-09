module Debug.Extra where

import Debug.Trace (trace)

observe :: (Show a) => a -> a
observe a = trace (show a) a
