module Data.Extra.List where

import Control.Monad (forM, forM_)
import Control.Monad.Random (RandomGen, getRandomR)
import Control.Monad.Random.Lazy (Rand)
import Control.Monad.ST (runST)
import Data.Monoid (Sum (Sum, getSum))
import Data.Vector (freeze, fromList, thaw, toList)
import qualified Data.Vector.Mutable as MV

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l
  | n > 0 = take n l : chunk n (drop n l)
  | otherwise = error "Negative or zero n"

count :: (a -> Bool) -> [a] -> Int
count f = getSum . foldMap (\a -> if f a then Sum 1 else mempty)

-- https://wiki.haskell.org/Random_shuffle adapted to vector
shuffle :: (RandomGen g) => [a] -> Rand g [a]
shuffle xs = do
  let l = length xs
  rands <- forM [0 .. (l - 2)] $ \i -> getRandomR (i, l - 1)
  let vec = runST $ do
        mvec <- thaw $ fromList xs
        forM_ (zip [0 ..] rands) $ \(i, j) -> do
          vi <- MV.read mvec i
          vj <- MV.read mvec j
          MV.write mvec j vi
          MV.write mvec i vj
        freeze mvec
  pure $ toList vec

-- like takeWhile but inclusive and inverted
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (h : t) = h : if f h then [] else takeUntil f t