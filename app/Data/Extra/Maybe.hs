module Data.Extra.Maybe where

import Control.Monad (ap)
import Data.Maybe (fromMaybe)

-- Because this was non-obvious to me, given:
-- fromMaybe :: c -> Maybe c -> c
-- ap :: Monad m => m (a -> b) -> m a -> m b

-- m is (c ->)
-- a is Maybe c
-- b is c
-- therefore:
-- m    (a       -> b) -> m    a         -> m    b  <-- ap signature
-- c -> (a       -> b) -> c -> a         -> c -> a  <-- substituting m : (c ->)
-- c -> (Maybe c -> c) -> c -> (Maybe c) -> c -> c  <-- substituting a : (Maybe c) & b : c

replace :: (a -> Maybe a) -> a -> a
replace = ap fromMaybe
