{-# LANGUAGE NamedFieldPuns #-}

-- | Declarative descriptions of game visuals
module Display.View where

import Data.Text (Text)
import Linear.V2 (V2 (V2))
import Resources (ImageKey)

-- One day we'll get serious about type-enforcing coordinate spaces, but not today
type Px = V2 Int

data TextAlignment = LeftAligned | RightAligned | Centered deriving (Show)

data Side = TopSide | BottomSide | LeftSide | RightSide deriving (Show)

data View
  = Group [View]
  | Translate Px View
  | Sprite ImageKey
  | Label TextAlignment Text
  | -- These are all absolute to the screen and reset the translation context
    CenterX View
  | CenterY View
  | From Side Int View
  deriving (Show)

data Primitive = SpritePrim ImageKey | TextPrim TextAlignment Text deriving (Show)

-- An AbsoluteView is an ordered list of primitives paired with their
-- position in terms of world space
type AbsoluteView = [(Px, Primitive)]

layout :: View -> Camera -> AbsoluteView
layout view Camera {cameraTranslation, cameraSize = (V2 cameraWidth cameraHeight)} = layout' cameraTranslation [] view
  where
    layout' :: Px -> AbsoluteView -> View -> AbsoluteView
    layout' offset acc view' = case view' of
      Group vs -> (layout' offset [] =<< vs) <> acc
      Translate translate v -> layout' (offset + translate) acc v
      Sprite imgKey -> (offset, SpritePrim imgKey) : acc
      Label a t -> (offset, TextPrim a t) : acc
      -- These reset the offset of the affected coordinate
      CenterX v -> layout' (px (div cameraWidth 2) (pxY offset)) acc v
      CenterY v -> layout' (px (pxX offset) (div cameraHeight 2)) acc v
      From LeftSide x v -> layout' (px x (pxY offset)) acc v
      From RightSide x v -> layout' (px (cameraWidth - x) (pxY offset)) acc v
      From BottomSide y v -> layout' (px (pxX offset) (cameraHeight - y)) acc v
      From TopSide y v -> layout' (px (pxX offset) y) acc v

data Camera = Camera
  { cameraTranslation :: Px,
    cameraScale :: Float,
    cameraSize :: V2 Int
  }
  deriving (Show)

type GameView = View

type UIView = View

layoutScreen :: GameView -> UIView -> Camera -> AbsoluteView
layoutScreen gv uiv cam = layout gv cam <> layout uiv (cam {cameraTranslation = px 0 0, cameraScale = 0})

px :: Int -> Int -> Px
px = V2

pxX :: Px -> Int
pxX (V2 x _) = x

pxY :: Px -> Int
pxY (V2 _ y) = y

empty :: View
empty = Group []

isEmpty :: View -> Bool
isEmpty (Group []) = True
isEmpty _ = False

translateRight :: Int -> View -> View
translateRight x = Translate (px x 0)

translateLeft :: Int -> View -> View
translateLeft x = Translate (px (-x) 0)

translateDown :: Int -> View -> View
translateDown y = Translate (px 0 y)

translateUp :: Int -> View -> View
translateUp y = Translate (px 0 (-y))

translateMap :: Int -> (Int -> Px) -> [View] -> [View]
translateMap step translateFn views =
  (\(i, v) -> Translate (translateFn i) v) <$> zip [0, step ..] views

hbox :: Int -> [View] -> View
hbox space = Group . translateMap space (px 0)

vbox :: Int -> [View] -> View
vbox space = Group . translateMap space (`px` 0)

gridLayout :: Int -> Int -> [[View]] -> View
gridLayout w h = vbox h . fmap (hbox w)

sparseGridLayout :: Int -> Int -> [((Int, Int), View)] -> View
sparseGridLayout w h views =
  Group
    [ Translate (px (x * w) (y * h)) view
      | ((x, y), view) <- views
    ]