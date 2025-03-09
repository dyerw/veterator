-- | This is its own module bc i didn't want to think about cyclic dependencies
module Veterator.Dir where

import Linear (V2 (..))
import Linear.Affine (Point (..), (.+^))

toPoint :: (Int, Int) -> Point V2 Int
toPoint (x, y) = P (V2 x y)

fromPoint :: Point V2 Int -> (Int, Int)
fromPoint (P (V2 x y)) = (x, y)

data Dir = N | NE | E | SE | S | SW | W | NW

move :: Dir -> (Int, Int) -> (Int, Int)
move N pos = fromPoint $ toPoint pos .+^ V2 0 (-1)
move NE pos = fromPoint $ toPoint pos .+^ V2 1 (-1)
move E pos = fromPoint $ toPoint pos .+^ V2 1 0
move SE pos = fromPoint $ toPoint pos .+^ V2 1 1
move S pos = fromPoint $ toPoint pos .+^ V2 0 1
move SW pos = fromPoint $ toPoint pos .+^ V2 (-1) 1
move W pos = fromPoint $ toPoint pos .+^ V2 (-1) 0
move NW pos = fromPoint $ toPoint pos .+^ V2 (-1) (-1)