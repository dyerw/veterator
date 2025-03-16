{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Declarative descriptions of game visuals
module Display.View where

import Data.Text (Text)
import FRP (keyPar)
import FRP.Yampa (SF, returnA, second)
import Linear.V2 (V2 (V2))
import Resources (ImageKey)

-- One day we'll get serious about type-enforcing coordinate spaces, but not today
type Px = V2 Int

data TextAlignment = LeftAligned | RightAligned | Centered deriving (Show)

data Side = TopSide | BottomSide | LeftSide | RightSide deriving (Show)

type Size = V2 Int

data Color = Color Int Int Int Int deriving (Show)

setOpacity :: Color -> Double -> Color
setOpacity (Color r g b _) o = Color r g b (round (o * 255))

black :: Color
black = Color 0 0 0 255

data View
  = Group [View]
  | Translate Px View
  | Sprite ImageKey
  | SheetSprite Int Int SpriteSheet
  | Label TextAlignment Text
  | Rect Size Color
  | -- These are all absolute to the screen and reset the translation context
    CenterX View
  | CenterY View
  | From Side Int View
  deriving (Show)

data SpriteSheet = SpriteSheet Int Int ImageKey deriving (Show)

data Primitive
  = SpritePrim ImageKey
  | SheetSpritePrim Int Int SpriteSheet
  | TextPrim TextAlignment Text
  | RectPrim Size Color
  deriving (Show)

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
      SheetSprite x y ss -> (offset, SheetSpritePrim x y ss) : acc
      Label a t -> (offset, TextPrim a t) : acc
      Rect s c -> (offset, RectPrim s c) : acc
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

sparseGridLayout :: Int -> Int -> [(V2 Int, View)] -> View
sparseGridLayout w h views =
  Group
    [ Translate (px (x * w) (y * h)) view
      | (V2 x y, view) <- views
    ]

gridSF :: (Ord k) => Int -> Int -> (a -> k) -> SF a View -> SF [(V2 Int, a)] View
gridSF w h keyFn viewSF = proc as -> do
  vs <- keyPar (keyFn . snd) (second viewSF) -< as
  returnA -< sparseGridLayout w h vs