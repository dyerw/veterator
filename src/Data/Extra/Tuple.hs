module Data.Extra.Tuple where

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)

toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)