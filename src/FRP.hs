{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module FRP where

import Control.Arrow (Arrow (arr), returnA, (>>>))
import Data.Extra.Tuple (toFst)
import Data.IdentityList (IdentityList)
import qualified Data.IdentityList as IL
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import FRP.Yampa (Event (Event), SF, after, isEvent, rpSwitch)
import FRP.Yampa.Loop (loopPre)
import FRP.Yampa.Switches (rpSwitchB)

keyPar :: forall k i o. (Ord k) => (i -> k) -> SF i o -> SF [i] [o]
keyPar keyFn sf = proc inputs -> do
  let nextSfs = M.fromList ((,sf) . keyFn <$> inputs)
  out <- mapSwitch -< (inputs, Event $ \sfs -> M.intersection (M.union sfs nextSfs) nextSfs)
  returnA -< (M.elems out)
  where
    mapSwitch =
      rpSwitch
        (\is sfMap -> M.intersectionWith (,) (M.fromList $ toFst keyFn <$> is) sfMap)
        M.empty

spawning :: forall e i o. (e -> SF i (Maybe o)) -> SF (i, [e]) [o]
spawning spawnFn = loopPre IL.empty step
  where
    step :: SF ((i, [e]), IdentityList (Maybe o)) ([o], IdentityList (Maybe o))
    step = proc ((input, events), prev) -> do
      let killF = killNothings prev
      let spawnF = spawnFromEvents events
      let updateList = killF . spawnF
      next <- rpSwitchB IL.empty -< (input, Event updateList)
      let output = catMaybes (IL.elems next)
      returnA -< (output, next)

    killNothings :: IdentityList (Maybe o) -> (IdentityList a -> IdentityList a)
    killNothings as = foldl (.) id (IL.map f as)
      where
        f :: (Int, Maybe o) -> (IdentityList a -> IdentityList a)
        f (_, Just _) = id
        f (i, Nothing) = IL.delete i

    spawnFromEvents :: [e] -> (IdentityList (SF i (Maybe o)) -> IdentityList (SF i (Maybe o)))
    spawnFromEvents events = foldl (.) id $ IL.insert . spawnFn <$> events

andDespawnAfterSecs :: Double -> SF a (Maybe a)
andDespawnAfterSecs secs = proc a -> do
  despawn <- after secs () -< ()
  if isEvent despawn
    then
      returnA -< Nothing
    else
      returnA -< Just a

liveUntil :: (a -> Bool) -> SF a (Maybe a)
liveUntil f = proc a -> do
  if f a
    then
      returnA -< Nothing
    else
      returnA -< Just a

withPrev :: SF (a, Maybe b) b -> SF a b
withPrev sf = loopPre Nothing (sf >>> arr (\b -> (b, Just b)))