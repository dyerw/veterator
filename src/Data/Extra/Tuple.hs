module Data.Extra.Tuple where

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapBoth :: (a -> c, b -> e) -> (a, b) -> (c, e)
mapBoth (fac, fbe) (a, b) = (fac a, fbe b)

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)

toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

tupLift :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
tupLift f (a1, a2) (b1, b2) = (f a1 b1, f a2 b2)

maybeSnd :: (a, Maybe b) -> Maybe (a, b)
maybeSnd (a, Just b) = Just (a, b)
maybeSnd (_, Nothing) = Nothing