module Data.IntervalSet (insert, Interval (..), IntervalSet, subsumes, empty) where

import Data.List (findIndex)

newtype IntervalSet a = IntervalSet [Interval a]

data Interval a = Interval a a

empty :: IntervalSet a
empty = IntervalSet []

insert :: (Ord a) => IntervalSet a -> Interval a -> IntervalSet a
insert (IntervalSet is) i = case findIndex (intervalOverlaps i) is of
  Just idx ->
    case splitAt idx is of
      (front, overlapper : back) -> IntervalSet $ front <> (joinIntervals overlapper i : back)
      _ -> undefined
  Nothing -> IntervalSet $ i : is

intervalContains :: (Ord a) => Interval a -> a -> Bool
intervalContains (Interval m n) a = a >= min m n && a <= max m n

intervalOverlaps :: (Ord a) => Interval a -> Interval a -> Bool
intervalOverlaps i (Interval m n) = i `intervalContains` m || i `intervalContains` n

intervalSubsumes :: (Ord a) => Interval a -> Interval a -> Bool
intervalSubsumes i (Interval m n) = i `intervalContains` m && i `intervalContains` n

subsumes :: (Ord a) => IntervalSet a -> Interval a -> Bool
subsumes (IntervalSet is) i = any (`intervalSubsumes` i) is

joinIntervals :: (Ord a) => Interval a -> Interval a -> Interval a
joinIntervals (Interval m1 n1) (Interval m2 n2) =
  let allBounds = [m1, n1, m2, n2]
   in Interval (foldr min m1 allBounds) (foldr max m1 allBounds)
