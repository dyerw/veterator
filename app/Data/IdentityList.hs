{-# LANGUAGE InstanceSigs #-}

module Data.IdentityList where

import Data.Bifunctor (Bifunctor (second))
import Data.List as List

data IdentityList a = IdentityList {nextKey :: Int, assocs :: [(Int, a)]} deriving (Show)

instance Functor IdentityList where
  fmap :: (a -> b) -> IdentityList a -> IdentityList b
  fmap f il = il {assocs = fmap (second f) (assocs il)}

empty :: IdentityList a
empty = IdentityList 0 []

insert :: a -> IdentityList a -> IdentityList a
insert a il =
  let key = nextKey il
   in il {nextKey = key + 1, assocs = (key, a) : assocs il}

fromList :: [a] -> IdentityList a
fromList = foldl (flip Data.IdentityList.insert) empty

elems :: IdentityList a -> [a]
elems = fmap snd . assocs

keys :: IdentityList a -> [Int]
keys = fmap fst . assocs

delete :: Int -> IdentityList a -> IdentityList a
delete i il = il {assocs = List.filter (\(i', _) -> i /= i') (assocs il)}

map :: ((Int, a) -> b) -> IdentityList a -> IdentityList b
map f il = il {assocs = fmap (\(i, a) -> (i, f (i, a))) (assocs il)}

filter :: (a -> Bool) -> IdentityList a -> IdentityList a
filter f il = il {assocs = List.filter (\(_, a) -> f a) (assocs il)}

length :: IdentityList a -> Int
length = List.length . assocs